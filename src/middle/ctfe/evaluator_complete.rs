//! Const expression evaluator for CTFE.
//!
//! This module defines `ConstEvaluator`, which evaluates constant
//! expressions and functions at compile time.

use crate::frontend::ast::{AstNode, MatchArm};

use super::context::ConstContext;
use super::value::ConstValue;
use super::error::{CtfeError, CtfeResult};
use super::ctfe_error;

/// Constant evaluator for compile-time evaluation
#[derive(Debug, Clone, Default)]
pub struct ConstEvaluator {
    /// Evaluation context
    context: ConstContext,
    /// Cache for evaluated expressions (for optimization)
    cache: std::collections::HashMap<AstNode, ConstValue>,
    /// Maximum evaluation depth (to prevent infinite recursion)
    max_depth: usize,
    /// Current evaluation depth
    current_depth: usize,
    /// Whether to enable caching
    enable_cache: bool,
}

impl ConstEvaluator {
    /// Create a new const evaluator with default settings
    pub fn new() -> Self {
        Self {
            context: ConstContext::new(),
            cache: std::collections::HashMap::new(),
            max_depth: 1000,
            current_depth: 0,
            enable_cache: true,
        }
    }

    /// Create a new const evaluator with custom max depth
    pub fn with_max_depth(max_depth: usize) -> Self {
        Self {
            max_depth,
            ..Self::new()
        }
    }

    /// Enable or disable caching
    pub fn set_cache_enabled(&mut self, enabled: bool) {
        self.enable_cache = enabled;
    }

    /// Clear the evaluation cache
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }

    /// Reset the evaluator state
    pub fn reset(&mut self) {
        self.context.clear();
        self.clear_cache();
        self.current_depth = 0;
    }

    /// Evaluate a program (list of AST nodes)
    pub fn evaluate_program(&mut self, asts: &[AstNode]) -> CtfeResult<Vec<AstNode>> {
        // First pass: register all const/comptime functions
        for ast in asts {
            if let AstNode::FuncDef {
                name,
                const_,
                comptime_,
                ..
            } = ast {
                if *const_ || *comptime_ {
                    self.context.register_function(name.clone(), ast.clone());
                }
            }
        }

        // Second pass: evaluate constants and transform AST
        let mut result = Vec::new();
        for ast in asts {
            match ast {
                AstNode::ConstDef {
                    name,
                    ty,
                    value,
                    attrs,
                    pub_,
                    comptime_,
                } => {
                    // Try to evaluate the constant
                    match self.eval_const_expr(value) {
                        Ok(ConstValue::Int(val)) => {
                            // Replace with a literal if evaluation succeeds
                            result.push(AstNode::ConstDef {
                                name: name.clone(),
                                ty: ty.clone(),
                                value: Box::new(AstNode::Lit(val)),
                                attrs: attrs.clone(),
                                pub_: *pub_,
                                comptime_: *comptime_,
                            });
                        }
                        Ok(ConstValue::UInt(val)) => {
                            // Convert to signed for now (TODO: proper unsigned support)
                            result.push(AstNode::ConstDef {
                                name: name.clone(),
                                ty: ty.clone(),
                                value: Box::new(AstNode::Lit(val as i64)),
                                attrs: attrs.clone(),
                                pub_: *pub_,
                                comptime_: *comptime_,
                            });
                        }
                        Ok(ConstValue::Bool(val)) => {
                            result.push(AstNode::ConstDef {
                                name: name.clone(),
                                ty: ty.clone(),
                                value: Box::new(AstNode::Bool(val)),
                                attrs: attrs.clone(),
                                pub_: *pub_,
                                comptime_: *comptime_,
                            });
                        }
                        Ok(_) => {
                            // Non-simple constant value - keep as-is for now
                            result.push(ast.clone());
                        }
                        Err(e) => {
                            // Keep as-is if evaluation fails
                            eprintln!("Warning: Could not evaluate constant {}: {}", name, e);
                            result.push(ast.clone());
                        }
                    }
                }
                _ => result.push(ast.clone()),
            }
        }

        Ok(result)
    }

    /// Evaluate a const expression to a ConstValue
    pub fn eval_const_expr(&mut self, expr: &AstNode) -> CtfeResult<ConstValue> {
        // Check recursion depth
        if self.current_depth >= self.max_depth {
            return Err(CtfeError::EvaluationTimeout);
        }
        
        self.current_depth += 1;
        let result = self.eval_const_expr_inner(expr);
        self.current_depth -= 1;
        
        result
    }

    /// Inner evaluation method (with depth tracking already handled)
    fn eval_const_expr_inner(&mut self, expr: &AstNode) -> CtfeResult<ConstValue> {
        // Check cache first (if enabled and expression is cacheable)
        if self.enable_cache && self.is_cacheable(expr) {
            if let Some(value) = self.cache.get(expr) {
                return Ok(value.clone());
            }
        }

        let value = match expr {
            // Literals
            AstNode::Lit(n) => Ok(ConstValue::Int(*n)),
            AstNode::Bool(b) => Ok(ConstValue::Bool(*b)),
            AstNode::Char(c) => Ok(ConstValue::Char(*c)),
            AstNode::StringLit(s) => Ok(ConstValue::String(s.clone())),
            
            // Binary operations
            AstNode::BinaryOp { op, left, right } => {
                self.eval_binary_op(op, left, right)
            }
            
            // Unary operations
            AstNode::UnaryOp { op, expr } => {
                self.eval_unary_op(op, expr)
            }
            
            // Variables
            AstNode::Var(name) => {
                self.eval_variable(name)
            }
            
            // Function calls
            AstNode::Call {
                receiver,
                method,
                args,
                ..
            } => {
                self.eval_function_call(receiver.as_ref(), method, args)
            }
            
            // Arrays
            AstNode::ArrayLit(elements) => {
                self.eval_array_literal(elements)
            }
            
            // Tuples
            AstNode::TupleLit(elements) => {
                self.eval_tuple_literal(elements)
            }
            
            // If expressions
            AstNode::If { cond, then, else_ } => {
                self.eval_if_expr(cond, then, else_.as_ref())
            }
            
            // Blocks
            AstNode::Block { body } => {
                self.eval_block(body)
            }
            
            // Let bindings
            AstNode::Let { mut_, pattern, ty: _, expr } => {
                self.eval_let_binding(*mut_, pattern, expr)
            }
            
            // Assignments
            AstNode::Assign(target, value_expr) => {
                self.eval_assignment(target, value_expr)
            }
            
            // Subscript/indexing
            AstNode::Subscript { base, index } => {
                self.eval_subscript(base, index)
            }
            
            // Match expressions
            AstNode::Match { expr, arms } => {
                self.eval_match_expr(expr, arms)
            }
            
            // Range expressions
            AstNode::Range { start, end, inclusive } => {
                self.eval_range_expr(start.as_ref(), end.as_ref(), *inclusive)
            }
            
            // For loops
            AstNode::For { pattern, expr, body } => {
                self.eval_for_loop(pattern, expr, body)
            }
            
            // While loops
            AstNode::While { cond, body } => {
                self.eval_while_loop(cond, body)
            }
            
            // Loop expressions
            AstNode::Loop { body } => {
                self.eval_loop_expr(body)
            }
            
            // Break expressions
            AstNode::Break(expr) => {
                self.eval_break_expr(expr.as_ref())
            }
            
            // Continue expressions
            AstNode::Continue => {
                self.eval_continue_expr()
            }
            
            // Return expressions
            AstNode::Return(expr) => {
                self.eval_return_expr(expr)
            }
            
            // Const definitions (already handled at program level)
            AstNode::ConstDef { value, .. } => {
                self.eval_const_expr(value)
            }
            
            // Unsupported expressions
            _ => Err(ctfe_error!(
                "unsupported expression in const context: {:?}",
                expr
            )),
        };

        // Cache the result if successful and cacheable
        if let (Ok(ref val), true) = (&value, self.enable_cache && self.is_cacheable(expr)) {
            self.cache.insert(expr.clone(), val.clone());
        }

        value
    }

    /// Check if an expression is cacheable
    fn is_cacheable(&self, expr: &AstNode) -> bool {
        // Don't cache expressions that can have side effects or depend on mutable state
        match expr {
            AstNode::Var(_) => false, // Variable values can change
            AstNode::Call { .. } => false, // Function calls may have side effects
            AstNode::Assign(_, _) => false, // Assignments have side effects
            AstNode::Let { .. } => false, // Let bindings have side effects
            AstNode::Block { .. } => false, // Blocks may have side effects
            AstNode::If { .. } => false, // If may have side effects in branches
            AstNode::Match { .. } => false, // Match may have side effects
            AstNode::For { .. } => false, // For loops have side effects
            AstNode::While { .. } => false, // While loops have side effects
            AstNode::Loop { .. } => false, // Loop expressions have side effects
            AstNode::Break(_) => false, // Break affects control flow
            AstNode::Continue => false, // Continue affects control flow
            AstNode::Return(_) => false, // Return affects control flow
            _ => true, // Pure expressions are cacheable
        }
    }

    /// Evaluate a binary operation
    fn eval_binary_op(&mut self, op: &str, left: &AstNode, right: &AstNode) -> CtfeResult<ConstValue> {
        let left_val = self.eval_const_expr(left)?;
        let right_val = self.eval_const_expr(right)?;
        
        left_val.binary_op(op, &right_val)
            .map_err(|e| CtfeError::InvalidOperation {
                op: op.to_string(),
                left_type: left_val.type_name().to_string(),
                right_type: Some(right_val.type_name().to_string()),
            })
    }

    /// Evaluate a unary operation
    fn eval_unary_op(&mut self, op: &str, expr: &AstNode) -> CtfeResult<ConstValue> {
        let val = self.eval_const_expr(expr)?;
        
        val.unary_op(op)
            .map_err(|e| CtfeError::InvalidOperation {
                op: op.to_string(),
                left_type: val.type_name().to_string(),
                right_type: None,
            })
    }

    /// Evaluate a variable reference
    fn eval_variable(&self, name: &str) -> CtfeResult<ConstValue> {
        // Check local variables first
        if let Some(value) = self.context.get_variable(name) {
            return Ok(value.clone());
        }
        
        // Check constants
        if let Some(value) = self.context.get_constant(name) {
            return Ok(value.clone());
        }
        
        Err(CtfeError::UndefinedVariable(name.to_string()))
    }

    /// Evaluate a function call
    fn eval_function_call(
        &mut self,
        receiver: Option<&AstNode>,
        method: &str,
        args: &[AstNode],
    ) -> CtfeResult<ConstValue> {
        // Method calls not supported yet
        if receiver.is_some() {
            return Err(ctfe_error!("method calls not supported in const context"));
        }

        // Check built-in functions first
        match method {
            "min" => self.eval_builtin_min(args),
            "max" => self.eval_builtin_max(args),
            "abs" => self.eval_builtin_abs(args),
            "size_of" => self.eval_builtin_size_of(args),
            "align_of" => self.eval_builtin_align_of(args),
            _ => self.eval_user_function_call(method, args),
        }
    }

    /// Evaluate built-in min function
    fn eval_builtin_min(&mut self, args: &[AstNode]) -> CtfeResult<ConstValue> {
        if args.len() != 2 {
            return Err(CtfeError::ArgumentCountMismatch {
                expected: 2,
                found: args.len(),
            });
        }
        
        let a = self.eval_const_expr(&args[0])?;
        let b = self.eval_const_expr(&args[1])?;
        
        match (a, b) {
            (ConstValue::Int(a_val), ConstValue::Int(b_val)) => {
                Ok(ConstValue::Int(a_val.min(b_val)))
            }
            (ConstValue::UInt(a_val), ConstValue::UInt(b_val)) => {
                Ok(ConstValue::UInt(a_val.min(b_val)))
            }
            (a_val, b_val) => Err(CtfeError::TypeMismatch {
                expected: "integer".to_string(),
                found: format!("{} and {}", a_val.type_name(), b_val.type_name()),
            }),
        }
    }

    /// Evaluate built-in max function
    fn eval_builtin_max(&mut self, args: &[AstNode]) -> CtfeResult<ConstValue> {
        if args.len() != 2 {
            return Err(CtfeError::ArgumentCountMismatch {
                expected: 2,
                found: args.len(),
            });
        }
        
        let a = self.eval_const_expr(&args[0])?;
        let b = self.eval_const_expr(&args[1])?;
        
        match (a, b) {
            (ConstValue::Int(a_val), ConstValue::Int(b_val)) => {
                Ok(ConstValue::Int(a_val.max(b_val)))
            }
            (ConstValue::UInt(a_val), ConstValue::UInt(b_val)) => {
                Ok(ConstValue::UInt(a_val.max(b_val)))
            }
            (a_val, b_val) => Err(CtfeError::TypeMismatch {
                expected: "integer".to_string(),
                found: format!("{} and {}", a_val.type_name(), b_val.type_name()),
            }),
        }
    }

    /// Evaluate built-in abs function
    fn eval_builtin_abs(&mut self, args: &[AstNode]) -> CtfeResult<ConstValue> {
        if args.len() != 1 {
            return Err(CtfeError::ArgumentCountMismatch {
                expected: 1,
                found: args.len(),
            });
        }
        
        let a = self.eval_const_expr(&args[0])?;
        
        match a {
            ConstValue::Int(a_val) => Ok(ConstValue::Int(a_val.abs())),
            ConstValue::UInt(a_val) => Ok(ConstValue::UInt(a_val)), // Already positive
            _ => Err(CtfeError::TypeMismatch {
                expected: "integer".to_string(),
                found: a.type_name().to_string(),
            }),
        }
    }

    /// Evaluate built-in size_of function
    fn eval_builtin_size_of(&mut self, _args: &[AstNode]) -> CtfeResult<ConstValue> {
        // TODO: Implement actual size_of based on type
        // For now, return placeholder values
        Ok(ConstValue::UInt(8)) // Assume 64-bit platform
    }

    /// Evaluate built-in align_of function
    fn eval_builtin_align_of(&mut self, _args: &[AstNode]) -> CtfeResult<ConstValue> {
        // TODO: Implement actual align_of based on type
        // For now, return placeholder values
        Ok(ConstValue::UInt(8)) // Assume 8-byte alignment
    }

    /// Evaluate user-defined function call
    fn eval_user_function_call(&mut self, name: &str, args: &[AstNode]) -> CtfeResult<ConstValue> {
        // Get function definition
        let func = self.context.get_function(name)
            .ok_or_else(|| CtfeError::FunctionNotFound(name.to_string()))?;
        
        // Check if it's a const/comptime function
        match func {
            AstNode::FuncDef {
                const_,
                comptime_,
                params,
                body,
                ret_expr,
                ..
            } => {
                if !*const_ && !*comptime_ {
                    return Err(ctfe_error!("function '{}' is not const or comptime", name));
                }
                
                // Enter function scope
                self.context.enter_const_fn();
                self.context.enter_scope(false); // Function scope is immutable
                
                // Bind arguments to parameters
                if params.len() != args.len() {
                    return Err(CtfeError::ArgumentCountMismatch {
                        expected: params.len(),
                        found: args.len(),
                    });
                }
                
                for (i, (param_name, _param_type)) in params.iter().enumerate() {
                    let arg_value = self.eval_const_expr(&args[i])?;
                    self.context.define_variable(param_name.clone(), arg_value)?;
                }
                
                // Evaluate function body
                let result = if let Some(expr) = ret_expr {
                    // Evaluate body statements first (for side effects)
                    for stmt in body {
                        self.eval_const_expr(stmt)?;
                    }
                    // Then evaluate return expression
                    self.eval_const_expr(expr)
                } else if !body.is_empty() {
                    // Evaluate statements, last expression is result
                    let mut last_value = ConstValue::Unit;
                    for stmt in body {
                        last_value =