//! Const expression evaluator for CTFE.
//!
//! This module defines `ConstEvaluator`, which evaluates constant
//! expressions and functions at compile time.

use crate::frontend::ast::AstNode;

use super::context::ConstContext;
use super::value::ConstValue;
use super::error::{CtfeError, CtfeResult};

/// Constant evaluator for compile-time evaluation
#[derive(Debug, Clone, Default)]
pub struct ConstEvaluator {
    /// Evaluation context
    context: ConstContext,
}

impl ConstEvaluator {
    /// Create a new const evaluator
    pub fn new() -> Self {
        Self {
            context: ConstContext::new(),
        }
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
        match expr {
            // Literals
            AstNode::Lit(n) => Ok(ConstValue::Int(*n)),
            AstNode::Bool(b) => Ok(ConstValue::Bool(*b)),
            
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
            
            // Const definitions
            AstNode::ConstDef { value, .. } => {
                self.eval_const_expr(value)
            }
            
            // Unsupported expressions
            _ => Err(CtfeError::UnsupportedExpression(
                format!("{:?}", expr)
            )),
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
            return Err(CtfeError::UnsupportedExpression(
                "method calls not supported in const context".to_string()
            ));
        }

        // Check built-in functions first
        match method {
            "min" => self.eval_builtin_min(args),
            "max" => self.eval_builtin_max(args),
            "abs" => self.eval_builtin_abs(args),
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
            _ => Err(CtfeError::TypeMismatch {
                expected: "integer".to_string(),
                found: a.type_name().to_string(),
            }),
        }
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
                    return Err(CtfeError::FunctionCallFailed(
                        format!("function '{}' is not const or comptime", name)
                    ));
                }
                
                // Enter function scope
                self.context.enter_const_fn();
                self.context.enter_scope(false);
                
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
                    // Evaluate body statements first
                    for stmt in body {
                        self.eval_const_expr(stmt)?;
                    }
                    // Then evaluate return expression
                    self.eval_const_expr(expr)
                } else if !body.is_empty() {
                    // Evaluate statements, last expression is result
                    let mut last_value = ConstValue::Unit;
                    for stmt in body {
                        last_value = self.eval_const_expr(stmt)?;
                    }
                    Ok(last_value)
                } else {
                    Ok(ConstValue::Unit)
                };
                
                // Exit function scope
                self.context.exit_scope()?;
                self.context.exit_const_fn();
                
                result
            }
            _ => Err(CtfeError::FunctionCallFailed(
                format!("expected function definition for '{}'", name)
            )),
        }
    }

    /// Evaluate an array literal
    fn eval_array_literal(&mut self, elements: &[AstNode]) -> CtfeResult<ConstValue> {
        let mut const_elements = Vec::new();
        for elem in elements {
            const_elements.push(self.eval_const_expr(elem)?);
        }
        Ok(ConstValue::Array(const_elements))
    }

    /// Evaluate an if expression
    fn eval_if_expr(
        &mut self,
        cond: &AstNode,
        then_branch: &[AstNode],
        else_branch: Option<&AstNode>,
    ) -> CtfeResult<ConstValue> {
        let cond_val = self.eval_const_expr(cond)?;
        let cond_bool = cond_val.as_bool()
            .ok_or_else(|| CtfeError::TypeMismatch {
                expected: "bool".to_string(),
                found: cond_val.type_name().to_string(),
            })?;
        
        if cond_bool {
            self.context.enter_scope(false);
            let result = self.eval_block(then_branch);
            self.context.exit_scope()?;
            result
        } else if let Some(else_expr) = else_branch {
            self.context.enter_scope(false);
            let result = self.eval_const_expr(else_expr);
            self.context.exit_scope()?;
            result
        } else {
            Ok(ConstValue::Unit)
        }
    }

    /// Evaluate a block
    fn eval_block(&mut self, body: &[AstNode]) -> CtfeResult<ConstValue> {
        self.context.enter_scope(false);
        let mut last_value = ConstValue::Unit;
        
        for stmt in body {
            last_value = self.eval_const_expr(stmt)?;
        }
        
        self.context.exit_scope()?;
        Ok(last_value)
    }

    /// Evaluate a let binding
    fn eval_let_binding(
        &mut self,
        mutable: bool,
        pattern: &AstNode,
        expr: &AstNode,
    ) -> CtfeResult<ConstValue> {
        let value = self.eval_const_expr(expr)?;
        
        // For now, only support simple variable patterns
        match pattern {
            AstNode::Var(name) => {
                self.context.define_variable(name.clone(), value.clone())?;
                Ok(value)
            }
            _ => Err(CtfeError::UnsupportedExpression(
                "complex patterns not supported in const let bindings".to_string()
            )),
        }
    }
}

/// Convenience function to evaluate a single const expression
pub fn eval_const_expr(expr: &AstNode) -> CtfeResult<ConstValue> {
    let mut evaluator = ConstEvaluator::new();
    evaluator.eval_const_expr(expr)
}

/// Convenience function to evaluate a program
pub fn evaluate_program(asts: &[AstNode]) -> CtfeResult<Vec<AstNode>> {
    let mut evaluator = ConstEvaluator::new();
    evaluator.evaluate_program(asts)
}