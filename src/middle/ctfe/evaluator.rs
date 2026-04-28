//! Const expression evaluator for CTFE.
//!
//! This module defines `ConstEvaluator`, which evaluates constant
//! expressions and functions at compile time.

use crate::frontend::ast::AstNode;

use super::context::ConstContext;
use super::value::ConstValue;
use super::error::{CtfeError, CtfeResult};
use super::visitor::{AstVisitor, AstTransformer};

/// Constant evaluator for compile-time evaluation
#[derive(Debug, Clone, Default)]
pub struct ConstEvaluator {
    /// Evaluation context
    context: ConstContext,
    /// Set when a Return statement is evaluated — used for early exit from function bodies
    returned_value: Option<ConstValue>,
    /// Recursion depth counter to prevent stack overflow on recursive comptime functions
    recursion_depth: u32,
}

impl ConstEvaluator {
    /// Check if an AST node contains variable references.
    /// Used to determine if eager CTFE evaluation is safe.
    fn contains_variable(node: &AstNode) -> bool {
        match node {
            AstNode::Var(_) => true,
            AstNode::BinaryOp { left, right, .. } => {
                Self::contains_variable(left) || Self::contains_variable(right)
            }
            AstNode::UnaryOp { expr, .. } => Self::contains_variable(expr),
            AstNode::Call { args, .. } => args.iter().any(|a| Self::contains_variable(a)),
            AstNode::Subscript { base, index, .. } => {
                Self::contains_variable(base) || Self::contains_variable(index)
            }
            _ => false,
        }
    }

    /// Create a new const evaluator
    pub fn new() -> Self {
        Self {
            context: ConstContext::new(),
            returned_value: None,
            recursion_depth: 0,
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
            } else {
            }
        }

        // Second pass: evaluate constants and transform AST
        let mut result = Vec::new();
        for ast in asts {
            let transformed = self.transform_ast_node(ast)?;
            result.push(transformed);
        }

        Ok(result)
    }

    /// Recursively transform an AST node, evaluating const expressions
    fn transform_ast_node(&mut self, node: &AstNode) -> CtfeResult<AstNode> {
        match node {
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
                        Ok(AstNode::ConstDef {
                            name: name.clone(),
                            ty: ty.clone(),
                            value: Box::new(AstNode::Lit(val)),
                            attrs: attrs.clone(),
                            pub_: *pub_,
                            comptime_: *comptime_,
                        })
                    }
                    Ok(ConstValue::Bool(val)) => {
                        Ok(AstNode::ConstDef {
                            name: name.clone(),
                            ty: ty.clone(),
                            value: Box::new(AstNode::Bool(val)),
                            attrs: attrs.clone(),
                            pub_: *pub_,
                            comptime_: *comptime_,
                        })
                    }
                    Ok(_) => {
                        // Non-simple constant value - keep as-is for now
                        Ok(node.clone())
                    }
                    Err(e) => {
                        // Keep as-is if evaluation fails
                        eprintln!("Warning: Could not evaluate constant {}: {}", name, e);
                        Ok(node.clone())
                    }
                }
            }
            AstNode::FuncDef {
                name,
                generics,
                lifetimes,
                params,
                ret,
                body,
                attrs,
                ret_expr,
                single_line,
                doc,
                pub_,
                async_,
                const_,
                comptime_,
                where_clauses,
            } => {
                // Transform function body
                let mut transformed_body = Vec::new();
                for stmt in body {
                    transformed_body.push(self.transform_ast_node(stmt)?);
                }
                
                let transformed_ret_expr = if let Some(expr) = ret_expr {
                    Some(Box::new(self.transform_ast_node(expr)?))
                } else {
                    None
                };
                
                Ok(AstNode::FuncDef {
                    name: name.clone(),
                    generics: generics.clone(),
                    lifetimes: lifetimes.clone(),
                    params: params.clone(),
                    ret: ret.clone(),
                    body: transformed_body,
                    attrs: attrs.clone(),
                    ret_expr: transformed_ret_expr,
                    single_line: *single_line,
                    doc: doc.clone(),
                    pub_: *pub_,
                    async_: *async_,
                    const_: *const_,
                    comptime_: *comptime_,
                    where_clauses: where_clauses.clone(),
                })
            }
            // Try to transform expressions
            AstNode::Lit(_)
            | AstNode::Bool(_)
            | AstNode::FloatLit(_)
            | AstNode::StringLit(_)
            | AstNode::Var(_)
            | AstNode::BinaryOp { .. }
            | AstNode::UnaryOp { .. }
            | AstNode::Call { .. }
            | AstNode::PathCall { .. }
            | AstNode::ArrayLit(_)
            | AstNode::If { .. }
            | AstNode::Block { .. }
            | AstNode::Let { .. }
            | AstNode::Return(_)
            | AstNode::ExprStmt { .. }
            | AstNode::Cast { .. }
            | AstNode::Closure { .. }
            | AstNode::Match { .. }
            | AstNode::Loop { .. }
            | AstNode::While { .. }
            | AstNode::For { .. }
            | AstNode::Break(_)
            | AstNode::Continue(_)
            | AstNode::ArrayRepeat { .. }
            | AstNode::DynamicArrayLit { .. }
            | AstNode::Await(_)
            | AstNode::Spawn { .. }
            | AstNode::Range { .. }
            | AstNode::TimingOwned { .. }
            | AstNode::Defer(_)
            | AstNode::TryProp { .. }
            | AstNode::DictLit { .. }
            | AstNode::Subscript { .. }
            | AstNode::FieldAccess { .. }
            | AstNode::FString(_)
            | AstNode::Assign(_, _)
            | AstNode::AssignOp { .. }
            | AstNode::IfLet { .. }
            | AstNode::Tuple(_)
            | AstNode::StructPattern { .. }
            | AstNode::StructLit { .. }
            | AstNode::RangePattern { .. }
            | AstNode::BindPattern { .. }
            | AstNode::OrPattern(_)
            | AstNode::TypeAnnotatedPattern { .. }
            | AstNode::Ignore
            | AstNode::Unsafe { .. }
            | AstNode::ComptimeBlock { .. }
            | AstNode::MacroCall { .. }
            | AstNode::MacroDef { .. }
            | AstNode::Use { .. }
            | AstNode::TypeAlias { .. }
            | AstNode::AssociatedType { .. } => {
                self.transform_expr(node)
            }
            // Top-level definitions that don't need transformation
            AstNode::Program(_)
            | AstNode::ConceptDef { .. }
            | AstNode::ImplBlock { .. }
            | AstNode::Method { .. }
            | AstNode::ExternFunc { .. }
            | AstNode::EnumDef { .. }
            | AstNode::StructDef { .. }
            | AstNode::ModDef { .. } => {
                // These are top-level definitions that don't contain runtime code
                // (or their bodies will be transformed recursively)
                Ok(node.clone())
            }
        }
    }

    /// Transform an expression node, attempting to evaluate it at compile time
    fn transform_expr(&mut self, node: &AstNode) -> CtfeResult<AstNode> {
        // First, recursively transform child nodes
        let transformed = match node {
            // Literals - already constant
            AstNode::Lit(n) => AstNode::Lit(*n),
            AstNode::Bool(b) => AstNode::Bool(*b),
            
            // Binary operation
            AstNode::BinaryOp { op, left, right } => {
                let transformed_left = self.transform_expr(left)?;
                let transformed_right = self.transform_expr(right)?;
                
                // Try to evaluate if both sides are now literals
                match (&transformed_left, &transformed_right) {
                    (AstNode::Lit(left_val), AstNode::Lit(right_val)) => {
                        // Evaluate the operation
                        match self.eval_binary_op(op, &transformed_left, &transformed_right) {
                            Ok(ConstValue::Int(result)) => AstNode::Lit(result),
                            Ok(ConstValue::Bool(result)) => AstNode::Bool(result),
                            _ => AstNode::BinaryOp {
                                op: op.clone(),
                                left: Box::new(transformed_left),
                                right: Box::new(transformed_right),
                            },
                        }
                    }
                    _ => AstNode::BinaryOp {
                        op: op.clone(),
                        left: Box::new(transformed_left),
                        right: Box::new(transformed_right),
                    },
                }
            }
            
            // Unary operation
            AstNode::UnaryOp { op, expr } => {
                let transformed_expr = self.transform_expr(expr)?;
                
                // Try to evaluate if operand is a literal
                match &transformed_expr {
                    AstNode::Lit(_) | AstNode::Bool(_) => {
                        match self.eval_unary_op(op, &transformed_expr) {
                            Ok(ConstValue::Int(result)) => {
                                AstNode::Lit(result)
                            }
                            Ok(ConstValue::Bool(result)) => {
                                AstNode::Bool(result)
                            }
                            Ok(_) => {
                                AstNode::UnaryOp {
                                    op: op.clone(),
                                    expr: Box::new(transformed_expr),
                                }
                            }
                            Err(e) => {
                                AstNode::UnaryOp {
                                    op: op.clone(),
                                    expr: Box::new(transformed_expr),
                                }
                            }
                        }
                    }
                    _ => {
                        AstNode::UnaryOp {
                            op: op.clone(),
                            expr: Box::new(transformed_expr),
                        }
                    }
                }
            }
            
            // Function call - check if it's a comptime function
            AstNode::Call {
                receiver,
                method,
                args,
                type_args,
                structural,
            } => {
                // Transform all arguments first
                let transformed_args: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| self.transform_expr(arg))
                    .collect();
                let transformed_args = transformed_args?;
                
                // Check if this is a comptime function call with no receiver.
                // Try to evaluate eagerly. If evaluation fails because variables
                // aren't bound yet (we're inside a function body being transformed),
                // just skip — the call will be evaluated when the outer function
                // is actually invoked.
                if receiver.is_none() {
                    let func_opt = self.context.get_function(method);
                    if let Some(func) = func_opt {
                        if let AstNode::FuncDef { const_, comptime_, .. } = func {
                            if *const_ || *comptime_ {
                                // Check if all args are simple (no variable references).
                                // If any arg references a variable, skip eager eval —
                                // we're inside a function body and vars aren't bound yet.
                                let has_vars = transformed_args.iter().any(|a| Self::contains_variable(a));
                                if !has_vars {
                                    match self.eval_function_call(None, method, &transformed_args) {
                                        Ok(ConstValue::Int(n)) => { return Ok(AstNode::Lit(n)); }
                                        Ok(ConstValue::Bool(b)) => { return Ok(AstNode::Bool(b)); }
                                        Ok(val) => { }
                                        Err(e) => { }
                                    }
                                }
                            }
                        }
                    }
                }
                
                // Return transformed call
                AstNode::Call {
                    receiver: receiver.clone(),
                    method: method.clone(),
                    args: transformed_args,
                    type_args: type_args.clone(),
                    structural: *structural,
                }
            }
            
            // If expression
            AstNode::If { cond, then, else_ } => {
                let transformed_cond = self.transform_expr(cond)?;
                let transformed_then: Result<Vec<_>, _> = then
                    .iter()
                    .map(|stmt| self.transform_expr(stmt))
                    .collect();
                let transformed_then = transformed_then?;
                let transformed_else: Result<Vec<_>, _> = else_
                    .iter()
                    .map(|stmt| self.transform_expr(stmt))
                    .collect();
                let transformed_else = transformed_else?;
                
                // Try to evaluate condition if it's a literal boolean
                if let AstNode::Bool(cond_val) = &transformed_cond {
                    if *cond_val {
                        // Condition is true, keep only then branch
                        if transformed_then.len() == 1 {
                            return Ok(transformed_then[0].clone());
                        } else {
                            // Multiple statements, wrap in block
                            return Ok(AstNode::Block { body: transformed_then });
                        }
                    } else if !transformed_else.is_empty() {
                        // Condition is false, keep only else branch
                        if transformed_else.len() == 1 {
                            return Ok(transformed_else[0].clone());
                        } else {
                            return Ok(AstNode::Block { body: transformed_else });
                        }
                    } else {
                        // False condition with no else branch
                        return Ok(AstNode::Block { body: vec![] });
                    }
                }
                
                AstNode::If {
                    cond: Box::new(transformed_cond),
                    then: transformed_then,
                    else_: transformed_else,
                }
            }
            
            // Block
            AstNode::Block { body } => {
                let transformed_body: Result<Vec<_>, _> = body
                    .iter()
                    .map(|stmt| self.transform_expr(stmt))
                    .collect();
                let transformed_body = transformed_body?;
                
                // If block has only one statement, unwrap it
                if transformed_body.len() == 1 {
                    transformed_body[0].clone()
                } else {
                    AstNode::Block { body: transformed_body }
                }
            }
            
            // Variable reference - check if it's a constant
            AstNode::Var(name) => {
                if let Some(constant) = self.context.get_constant(name) {
                    // Replace variable with its constant value
                    match constant {
                        ConstValue::Int(val) => AstNode::Lit(val),
                        ConstValue::Bool(val) => AstNode::Bool(val),
                        _ => AstNode::Var(name.clone()),
                    }
                } else {
                    AstNode::Var(name.clone())
                }
            }
            
            // While loop - transform condition and body
            AstNode::While { cond, body } => {
                let transformed_cond = self.transform_expr(cond)?;
                let transformed_body: Result<Vec<_>, _> = body
                    .iter()
                    .map(|stmt| self.transform_expr(stmt))
                    .collect();
                let transformed_body = transformed_body?;
                
                AstNode::While {
                    cond: Box::new(transformed_cond),
                    body: transformed_body,
                }
            }
            
            // For loop - transform pattern, expression, and body
            AstNode::For { pattern, expr, body } => {
                let transformed_pattern = self.transform_expr(pattern)?;
                let transformed_expr = self.transform_expr(expr)?;
                let transformed_body: Result<Vec<_>, _> = body
                    .iter()
                    .map(|stmt| self.transform_expr(stmt))
                    .collect();
                let transformed_body = transformed_body?;
                
                AstNode::For {
                    pattern: Box::new(transformed_pattern),
                    expr: Box::new(transformed_expr),
                    body: transformed_body,
                }
            }
            
            // Loop statement - transform body
            AstNode::Loop { body } => {
                let transformed_body: Result<Vec<_>, _> = body
                    .iter()
                    .map(|stmt| self.transform_expr(stmt))
                    .collect();
                let transformed_body = transformed_body?;
                
                AstNode::Loop { body: transformed_body }
            }
            
            // Break statement - transform optional expression
            AstNode::Break(expr_opt) => {
                let transformed_expr_opt = if let Some(expr) = expr_opt {
                    Some(Box::new(self.transform_expr(expr)?))
                } else {
                    None
                };
                AstNode::Break(transformed_expr_opt)
            }
            
            // Continue statement - transform optional expression
            AstNode::Continue(expr_opt) => {
                let transformed_expr_opt = if let Some(expr) = expr_opt {
                    Some(Box::new(self.transform_expr(expr)?))
                } else {
                    None
                };
                AstNode::Continue(transformed_expr_opt)
            }
            
            // Return statement - transform expression
            AstNode::Return(expr) => {
                let transformed_expr = self.transform_expr(expr)?;
                AstNode::Return(Box::new(transformed_expr))
            }
            
            // Let binding - transform pattern and expression
            AstNode::Let { mut_, pattern, ty, expr } => {
                let transformed_pattern = self.transform_expr(pattern)?;
                let transformed_expr = self.transform_expr(expr)?;
                AstNode::Let {
                    mut_: *mut_,
                    pattern: Box::new(transformed_pattern),
                    ty: ty.clone(),
                    expr: Box::new(transformed_expr),
                }
            }
            
            // Array literal - transform elements
            AstNode::ArrayLit(elements) => {
                let transformed_elements: Result<Vec<_>, _> = elements
                    .iter()
                    .map(|elem| self.transform_expr(elem))
                    .collect();
                let transformed_elements = transformed_elements?;
                AstNode::ArrayLit(transformed_elements)
            }
            
            // Expression statement - transform inner expression
            AstNode::ExprStmt { expr } => {
                let transformed_expr = self.transform_expr(expr)?;
                // Return the transformed expression (dropping ExprStmt wrapper)
                // because in CTFE we want constant folding, and a bare expression
                // can serve as a statement (implicit return).
                transformed_expr
            }
            
            // For other nodes, recursively transform children but keep structure
            _ => {
                // Default: clone the node (to be extended for other node types)
                node.clone()
            }
        };
        
        Ok(transformed)
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
                self.eval_function_call(receiver.as_deref(), method, args)
            }
            
            // Arrays
            AstNode::ArrayLit(elements) => {
                self.eval_array_literal(elements)
            }
            
            // Expression statements — unwrap and evaluate inner expression
            AstNode::ExprStmt { expr } => {
                self.eval_const_expr(expr)
            }
            
            // If expressions
            AstNode::If { cond, then, else_ } => {
                self.eval_if_expr_with_else(cond, then, else_)
            }
            
            // Blocks
            AstNode::Block { body } => {
                self.eval_block(body)
            }
            
            // Let bindings
            AstNode::Let { mut_, pattern, ty: _, expr } => {
                self.eval_let_binding(*mut_, pattern, expr)
            }
            
            // Assignment expressions
            AstNode::Assign(target, value) => {
                self.eval_assignment(target, value)
            }
            
            // Compound assignment expressions: desugar to target = target op value
            AstNode::AssignOp { op, target, value } => {
                let lhs = target.as_ref().clone();
                let rhs = AstNode::BinaryOp {
                    op: op.clone(),
                    left: Box::new(lhs),
                    right: value.clone(),
                };
                self.eval_assignment(target, &rhs)
            }
            
            // Const definitions
            AstNode::ConstDef { value, .. } => {
                self.eval_const_expr(value)
            }
            
            // While loops
            AstNode::While { cond, body } => {
                self.eval_while_loop(cond, body)
            }
            
            // Loop statements
            AstNode::Loop { body } => {
                self.eval_loop(body)
            }
            
            // For loops
            AstNode::For { pattern, expr, body } => {
                self.eval_for_loop(pattern, expr, body)
            }
            
            // Return expressions — evaluate the inner value and signal early exit
            AstNode::Return(expr) => {
                let val = self.eval_const_expr(expr)?;
                self.returned_value = Some(val.clone());
                Ok(val)
            }
            

            // Array subscript access: array[index]
            AstNode::Subscript { base, index } => {
                let index_val = self.eval_const_expr(index)?;
                
                let idx = index_val.as_int().ok_or_else(|| CtfeError::TypeMismatch {
                    expected: "integer index".to_string(),
                    found: index_val.type_name().to_string(),
                })?;
                
                if idx < 0 {
                    return Err(CtfeError::InvalidOperation {
                        op: "array_index".to_string(),
                        left_type: "".to_string(),
                        right_type: None,
                    });
                }
                
                // Fast path: if base is a simple variable, use reference-based lookup
                if let AstNode::Var(name) = base.as_ref() {
                    self.context.get_array_element(name, idx as usize)
                } else {
                    let base_val = self.eval_const_expr(base)?;
                    base_val.array_index(idx as usize).cloned()
                }
            }
            
            // Array repeat literal: [value; size]
            AstNode::ArrayRepeat { value, size } => {
                let val = self.eval_const_expr(value)?;
                let size_val = self.eval_const_expr(size)?;
                
                let count = size_val.as_int().ok_or_else(|| CtfeError::TypeMismatch {
                    expected: "integer size".to_string(),
                    found: size_val.type_name().to_string(),
                })?;
                
                if count < 0 {
                    return Err(CtfeError::InvalidOperation {
                        op: "array_repeat".to_string(),
                        left_type: "".to_string(),
                        right_type: None,
                    });
                }
                
                let mut elements = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    elements.push(val.clone());
                }
                // Auto-convert to IntArray if repeated value is Int
                if let ConstValue::Int(_) = val {
                    let int_val = val.as_int().unwrap();
                    let ints = vec![int_val; count as usize];
                    return Ok(ConstValue::IntArray(ints));
                }
                Ok(ConstValue::Array(elements))
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
        
        let result = val.unary_op(op)
            .map_err(|e| CtfeError::InvalidOperation {
                op: op.to_string(),
                left_type: val.type_name().to_string(),
                right_type: None,
            });
        result
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
        // Recursion depth limit to prevent stack overflow
        const MAX_RECURSION_DEPTH: u32 = 1024;
        if self.recursion_depth >= MAX_RECURSION_DEPTH {
            return Err(CtfeError::FunctionCallFailed(
                format!("recursion depth limit ({}) exceeded for '{}'", MAX_RECURSION_DEPTH, name)
            ));
        }
        self.recursion_depth += 1;
        
        // Get function definition
        let func = self.context.get_function(name)
            .cloned()
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
                if !const_ && !comptime_ {
                    return Err(CtfeError::FunctionCallFailed(
                        format!("function '{}' is not const or comptime", name)
                    ));
                }
                

                
                // Enter function scope
                self.context.enter_const_fn();
                let _ = self.context.enter_scope(false);
                
                // Bind arguments to parameters
                if params.len() != args.len() {
                    return Err(CtfeError::ArgumentCountMismatch {
                        expected: params.len(),
                        found: args.len(),
                    });
                }
                
                // Evaluate ALL arguments first before binding ANY parameter.
                // This prevents variable shadowing: args[1] (like n*acc) must
                // see the PARENT's n and acc, not the current function's n.
                let mut arg_values = Vec::with_capacity(args.len());
                for i in 0..args.len() {
                    arg_values.push(self.eval_const_expr(&args[i])?);
                }
                for (i, (param_name, _param_type)) in params.iter().enumerate() {
                    self.context.define_variable(param_name.clone(), arg_values[i].clone())?;
                }
                
                // Reset returned_value before entering function body
                self.returned_value = None;
                
                // Evaluate function body
                let result = if let Some(expr) = ret_expr {
                    // Evaluate body statements first
                    for stmt in body.iter() {
                        self.eval_const_expr(stmt)?;
                        if self.returned_value.is_some() {
                            break;
                        }
                    }
                    // Then evaluate return expression (if we didn't already return)
                    if self.returned_value.is_some() {
                        Ok(self.returned_value.take().unwrap())
                    } else {
                        self.eval_const_expr(&expr)
                    }
                } else if !body.is_empty() {
                    // Evaluate statements, respecting Return as early exit
                    let mut last_value = ConstValue::Unit;
                    for stmt in body.iter() {
                        last_value = self.eval_const_expr(stmt)?;
                        if self.returned_value.is_some() {
                            break;
                        }
                    }
                    if let Some(ret_val) = self.returned_value.take() {
                        Ok(ret_val)
                    } else {
                        Ok(last_value)
                    }
                } else {
                    Ok(ConstValue::Unit)
                };
                
                // Exit function scope
                self.context.exit_scope()?;
                self.context.exit_const_fn();
                
                // Decrement recursion depth
                self.recursion_depth = self.recursion_depth.saturating_sub(1);
                
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
        // Auto-convert to IntArray if all elements are Int
        if const_elements.iter().all(|e| matches!(e, ConstValue::Int(_))) {
            let ints: Vec<i64> = const_elements.iter().map(|e| e.as_int().unwrap()).collect();
            return Ok(ConstValue::IntArray(ints));
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
        // Accept either Bool or Int (non-zero = true)
        let cond_bool = match cond_val {
            ConstValue::Bool(b) => b,
            ConstValue::Int(i) => i != 0,
            _ => return Err(CtfeError::TypeMismatch {
                expected: "bool or int".to_string(),
                found: cond_val.type_name().to_string(),
            }),
        };
        
        if cond_bool {
            let then_needs_scope = Self::body_has_let_decls(then_branch);
            if then_needs_scope {
                let _ = self.context.enter_scope(false);
            }
            let result = self.eval_block(then_branch);
            if then_needs_scope {
                self.context.exit_scope()?;
            }
            result
        } else if let Some(else_expr) = else_branch {
            // else branch is a single expression, not a block — it can't have let declarations
            // so we always skip scope management for it
            let result = self.eval_const_expr(else_expr);
            result
        } else {
            Ok(ConstValue::Unit)
        }
    }

    /// Evaluate an if expression with the full else block (vector of statements).
    /// This properly handles else branches that contain multiple statements
    /// (wrapped in ExprStmt or other statement types).
    fn eval_if_expr_with_else(
        &mut self,
        cond: &AstNode,
        then_branch: &[AstNode],
        else_branch: &[AstNode],
    ) -> CtfeResult<ConstValue> {
        let cond_val = self.eval_const_expr(cond)?;
        let cond_bool = match cond_val {
            ConstValue::Bool(b) => b,
            ConstValue::Int(i) => i != 0,
            _ => return Err(CtfeError::TypeMismatch {
                expected: "bool or int".to_string(),
                found: cond_val.type_name().to_string(),
            }),
        };
        
        if cond_bool {
            let then_needs_scope = Self::body_has_let_decls(then_branch);
            if then_needs_scope {
                let _ = self.context.enter_scope(false);
            }
            let result = self.eval_block(then_branch);
            if then_needs_scope {
                self.context.exit_scope()?;
            }
            result
        } else if !else_branch.is_empty() {
            let else_needs_scope = Self::body_has_let_decls(else_branch);
            if else_needs_scope {
                let _ = self.context.enter_scope(false);
            }
            let result = self.eval_block(else_branch);
            if else_needs_scope {
                self.context.exit_scope()?;
            }
            result
        } else {
            Ok(ConstValue::Unit)
        }
    }

    /// Check if a list of AST nodes contains any `let` declarations.
    /// Used to skip scope management overhead for blocks without local variables.
    fn body_has_let_decls(body: &[AstNode]) -> bool {
        body.iter().any(|stmt| matches!(stmt, AstNode::Let { .. }))
    }

    /// Evaluate a block
    fn eval_block(&mut self, body: &[AstNode]) -> CtfeResult<ConstValue> {
        let needs_scope = Self::body_has_let_decls(body);
        if needs_scope {
            let _ = self.context.enter_scope(false);
        }
        let mut last_value = ConstValue::Unit;
        
        for stmt in body {
            last_value = self.eval_const_expr(stmt)?;
            if self.returned_value.is_some() {
                break;
            }
        }
        
        if needs_scope {
            self.context.exit_scope()?;
        }
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
        // Unwrap TypeAnnotatedPattern to get the inner pattern
        let inner_pattern = match pattern {
            AstNode::TypeAnnotatedPattern { pattern: inner, .. } => inner,
            other => other,
        };
        
        match inner_pattern {
            AstNode::Var(name) => {

                self.context.define_variable(name.clone(), value.clone())?;
                Ok(value)
            }
            _ => Err(CtfeError::UnsupportedExpression(
                "complex patterns not supported in const let bindings".to_string()
            )),
        }
    }

    /// Evaluate an assignment expression
    fn eval_assignment(
        &mut self,
        target: &AstNode,
        value: &AstNode,
    ) -> CtfeResult<ConstValue> {
        let val = self.eval_const_expr(value)?;
        
        // For now, only support simple variable assignments
        match target {
            // Array element assignment: array[index] = value
            AstNode::Subscript { base, index } => {
                let base_var = match base.as_ref() {
                    AstNode::Var(name) => name.clone(),
                    other => return Err(CtfeError::UnsupportedExpression(
                        format!("complex array subscript target not supported: {:?}", other)
                    )),
                };
                
                let index_val = self.eval_const_expr(index)?;
                let idx = index_val.as_int().ok_or_else(|| CtfeError::TypeMismatch {
                    expected: "integer index".to_string(),
                    found: index_val.type_name().to_string(),
                })?;
                
                if idx < 0 {
                    return Err(CtfeError::InvalidOperation {
                        op: "array_set".to_string(),
                        left_type: "".to_string(),
                        right_type: None,
                    });
                }
                
                // Fast path: mutate array element in-place without cloning the whole array
                self.context.assign_array_element(&base_var, idx as usize, val.clone())?;
                Ok(val)
            }
            AstNode::Var(name) => {

                self.context.assign_variable(name, val.clone())?;
                Ok(val)
            }
            _ => Err(CtfeError::UnsupportedExpression(
                "complex assignment targets not supported in const context".to_string()
            )),
        }
    }

    /// Evaluate a while loop at compile time
    fn eval_while_loop(&mut self, cond: &AstNode, body: &[AstNode]) -> CtfeResult<ConstValue> {
        let mut loop_count = 0;
        const MAX_LOOP_ITERATIONS: usize = 10000000;
        

        loop {
            if loop_count >= MAX_LOOP_ITERATIONS {
                return Err(CtfeError::LoopTooManyIterations);
            }
            
            // Evaluate condition — accept either Bool or Int (non-zero = true)
            let cond_val = self.eval_const_expr(cond)?;
            let cond_bool = match cond_val {
                ConstValue::Bool(b) => b,
                ConstValue::Int(i) => i != 0,
                _ => return Err(CtfeError::TypeMismatch {
                    expected: "bool or int".to_string(),
                    found: cond_val.type_name().to_string(),
                }),
            };
            

            
            if !cond_bool {
                break;
            }
            
            // Evaluate body (no per-iteration scope overhead — outer scope already exists)
            let mut _last_value = ConstValue::Unit;
            for stmt in body {
                _last_value = self.eval_const_expr(stmt)?;
            }
            
            loop_count += 1;
        }
        

        Ok(ConstValue::Unit)
    }

    /// Evaluate a loop statement (infinite loop) at compile time
    fn eval_loop(&mut self, body: &[AstNode]) -> CtfeResult<ConstValue> {
        // For now, we can't evaluate infinite loops at compile time
        // unless they contain a break statement with constant condition
        // This is complex, so return unsupported for now
        Err(CtfeError::UnsupportedExpression(
            "loop statements not supported in const context".to_string()
        ))
    }

    /// Evaluate a for loop at compile time
    fn eval_for_loop(&mut self, pattern: &AstNode, expr: &AstNode, body: &[AstNode]) -> CtfeResult<ConstValue> {
        // Extract the range expression: start..end
        // For loops like: for i in start..end { body }
        let (start_expr, end_expr, inclusive) = match expr {
            AstNode::Range { start, end, inclusive } => (start.as_ref(), end.as_ref(), *inclusive),
            AstNode::BinaryOp { op, left, right, .. } if op == ".." => {
                (left.as_ref(), right.as_ref(), false)
            }
            _ => {
                return Err(CtfeError::UnsupportedExpression(
                    format!("for loop range must be start..end or start..=end, got: {:?}", expr)
                ));
            }
        };

        // Evaluate start and end
        let start_val = self.eval_const_expr(start_expr)?;
        let end_val = self.eval_const_expr(end_expr)?;

        let start = start_val.as_int().ok_or_else(|| CtfeError::TypeMismatch {
            expected: "integer".to_string(),
            found: start_val.type_name().to_string(),
        })?;
        let end = end_val.as_int().ok_or_else(|| CtfeError::TypeMismatch {
            expected: "integer".to_string(),
            found: end_val.type_name().to_string(),
        })?;

        // Extract variable name from pattern
        let var_name = match pattern {
            AstNode::Var(name) => name.clone(),
            _ => {
                return Err(CtfeError::UnsupportedExpression(
                    "for loop variable pattern must be a simple variable".to_string()
                ));
            }
        };

        let mut loop_count: u64 = 0;
        const MAX_LOOP_ITERATIONS: usize = 10000000;

        let range_end = if inclusive { end + 1 } else { end };

        for i in start..range_end {
            if loop_count as usize >= MAX_LOOP_ITERATIONS {
                return Err(CtfeError::LoopTooManyIterations);
            }

            // Bind loop variable
            self.context.define_variable(var_name.clone(), ConstValue::Int(i))?;

            // Evaluate body
            for stmt in body {
                self.eval_const_expr(stmt)?;
            }

            loop_count += 1;
        }

        Ok(ConstValue::Unit)
    }

    /// Try to evaluate a const function call (legacy compatibility)
    pub fn try_eval_const_call(
        &mut self,
        func: &AstNode,
        args: &[AstNode],
    ) -> CtfeResult<ConstValue> {
        // Extract function name from FuncDef
        match func {
            AstNode::FuncDef { name, .. } => {
                // Register the function if not already registered
                if !self.context.get_function(name).is_some() {
                    self.context.register_function(name.clone(), func.clone());
                }
                // Evaluate the function call
                self.eval_user_function_call(name, args)
            }
            _ => Err(CtfeError::UnsupportedExpression(
                "try_eval_const_call requires a FuncDef node".to_string(),
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

impl AstVisitor for ConstEvaluator {
    fn visit(&mut self, node: &AstNode) -> CtfeResult<()> {
        // Default implementation uses visit_children
        self.visit_children(node)
    }
}

impl AstTransformer for ConstEvaluator {
    fn transform(&mut self, node: AstNode) -> CtfeResult<AstNode> {
        // Delegate to existing transform methods
        match &node {
            AstNode::ConstDef { .. } => self.transform_ast_node(&node),
            AstNode::FuncDef { .. } => self.transform_ast_node(&node),
            _ => self.transform_expr(&node),
        }
    }
}
