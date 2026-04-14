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
                    println!("DEBUG: Evaluating constant {} with value: {:?}", name, value);
                    match self.eval_const_expr(value) {
                        Ok(ConstValue::Int(val)) => {
                            println!("DEBUG: Constant {} evaluated to Int({})", name, val);
                            // Store the value in context for other constants to reference
                            self.context.set_global(name.clone(), ConstValue::Int(val));
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
                            // Store the value in context for other constants to reference
                            self.context.set_global(name.clone(), ConstValue::UInt(val));
                            // For unsigned integers, we need to handle them differently
                            // since AST only has Lit(i64). We'll convert if it fits.
                            if val <= i64::MAX as u64 {
                                result.push(AstNode::ConstDef {
                                    name: name.clone(),
                                    ty: ty.clone(),
                                    value: Box::new(AstNode::Lit(val as i64)),
                                    attrs: attrs.clone(),
                                    pub_: *pub_,
                                    comptime_: *comptime_,
                                });
                            } else {
                                // Keep as-is if it doesn't fit in i64
                                result.push(ast.clone());
                            }
                        }
                        Ok(ConstValue::Bool(val)) => {
                            // Store the value in context for other constants to reference
                            self.context.set_global(name.clone(), ConstValue::Bool(val));
                            result.push(AstNode::ConstDef {
                                name: name.clone(),
                                ty: ty.clone(),
                                value: Box::new(AstNode::Bool(val)),
                                attrs: attrs.clone(),
                                pub_: *pub_,
                                comptime_: *comptime_,
                            });
                        }
                        Ok(ConstValue::Array(arr)) => {
                            // Convert array to array literal if all elements are simple
                            let mut all_simple = true;
                            let mut elements = Vec::new();
                            
                            for val in arr {
                                match val {
                                    ConstValue::Int(n) => {
                                        elements.push(AstNode::Lit(n));
                                    }
                                    ConstValue::UInt(n) => {
                                        if n <= i64::MAX as u64 {
                                            elements.push(AstNode::Lit(n as i64));
                                        } else {
                                            all_simple = false;
                                            break;
                                        }
                                    }
                                    ConstValue::Bool(b) => {
                                        elements.push(AstNode::Bool(b));
                                    }
                                    _ => {
                                        all_simple = false;
                                        break;
                                    }
                                }
                            }
                            
                            if all_simple {
                                // Create array literal
                                result.push(AstNode::ConstDef {
                                    name: name.clone(),
                                    ty: ty.clone(),
                                    value: Box::new(AstNode::ArrayLit(elements)),
                                    attrs: attrs.clone(),
                                    pub_: *pub_,
                                    comptime_: *comptime_,
                                });
                            } else {
                                // Keep as-is
                                result.push(ast.clone());
                            }
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
        println!("DEBUG eval_const_expr: {:?}", expr);
        match expr {
            // Literals
            AstNode::Lit(n) => {
                println!("DEBUG: Literal value: {}", n);
                Ok(ConstValue::Int(*n))
            }
            AstNode::Bool(b) => Ok(ConstValue::Bool(*b)),
            
            // Variables
            AstNode::Var(name) => self.context.lookup_variable(name),
            
            // Binary operations
            AstNode::BinaryOp { op, left, right } => {
                let left_val = self.eval_const_expr(left)?;
                let right_val = self.eval_const_expr(right)?;
                
                match left_val.binary_op(op, &right_val) {
                    Ok(val) => Ok(val),
                    Err(msg) => ctfe_error!(UnsupportedOperation, msg),
                }
            }
            
            // Unary operations
            AstNode::UnaryOp { op, expr: inner } => {
                let val = self.eval_const_expr(inner)?;
                match val.unary_op(op) {
                    Ok(result) => Ok(result),
                    Err(msg) => ctfe_error!(UnsupportedOperation, msg),
                }
            }
            
            // Call expressions (function calls)
            AstNode::Call { receiver, method, args, .. } => {
                // For now, only handle simple function calls without receiver
                if receiver.is_none() {
                    self.try_eval_const_call_by_name(method, args)
                } else {
                    ctfe_error!(UnsupportedOperation, format!("method calls not supported in CTFE"))
                }
            }
            
            // Array literals
            AstNode::ArrayLit(elements) => {
                let mut arr = Vec::new();
                for elem in elements {
                    arr.push(self.eval_const_expr(elem)?);
                }
                Ok(ConstValue::Array(arr))
            }
            
            // Return statements
            AstNode::Return(expr) => {
                self.eval_const_expr(expr)
            }
            
            // Unsupported expressions
            _ => ctfe_error!(NotConstant),
        }
    }

    /// Try to evaluate a const function call by name
    pub fn try_eval_const_call_by_name(
        &mut self,
        name: &str,
        args: &[AstNode],
    ) -> CtfeResult<ConstValue> {
        let func = self.context.lookup_function(name)?.clone();
        self.try_eval_const_call(&func, args)
    }

    /// Try to evaluate a const function call
    pub fn try_eval_const_call(
        &mut self,
        func: &AstNode,
        args: &[AstNode],
    ) -> CtfeResult<ConstValue> {
        if let AstNode::FuncDef {
            params,
            body,
            ret_expr,
            ..
        } = func {
            // Evaluate arguments
            let mut arg_values = Vec::new();
            for arg in args {
                arg_values.push(self.eval_const_expr(arg)?);
            }
            
            // Enter new scope for function evaluation
            self.context.enter_scope();
            
            // Bind parameters to arguments
            for (i, (param_name, _)) in params.iter().enumerate() {
                if i < arg_values.len() {
                    self.context.define_variable(param_name.clone(), arg_values[i].clone())?;
                }
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
                    last_value = self.eval_const_expr(stmt)?;
                }
                Ok(last_value)
            } else {
                Ok(ConstValue::Unit)
            };
            
            self.context.exit_scope()?;
            result
        } else {
            ctfe_error!(UnsupportedOperation, "not a function".to_string())
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

/// Evaluate constants in a program
pub fn evaluate_constants(asts: &[AstNode]) -> CtfeResult<Vec<AstNode>> {
    evaluate_program(asts)
}