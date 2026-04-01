// src/middle/const_eval.rs
//! Compile-Time Function Evaluation (CTFE) for Zeta v0.5.0
//!
//! Evaluates const functions at compile time.

use crate::frontend::ast::AstNode;
use std::collections::HashMap;

/// Constant evaluator for compile-time evaluation
pub struct ConstEvaluator {
    /// Cache of evaluated constants
    cache: HashMap<AstNode, i64>,
}

impl Default for ConstEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstEvaluator {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    /// Evaluate a const expression
    pub fn eval_const_expr(&mut self, expr: &AstNode) -> Result<i64, String> {
        // Check cache first
        if let Some(&value) = self.cache.get(expr) {
            return Ok(value);
        }

        let value = match expr {
            AstNode::Lit(n) => *n,
            AstNode::BinaryOp { op, left, right } => {
                let left_val = self.eval_const_expr(left)?;
                let right_val = self.eval_const_expr(right)?;

                match op.as_str() {
                    "+" => left_val + right_val,
                    "-" => left_val - right_val,
                    "*" => left_val * right_val,
                    "/" => {
                        if right_val == 0 {
                            return Err("Division by zero in const expression".to_string());
                        }
                        left_val / right_val
                    }
                    "%" => {
                        if right_val == 0 {
                            return Err("Modulo by zero in const expression".to_string());
                        }
                        left_val % right_val
                    }
                    "<<" => left_val << right_val,
                    ">>" => left_val >> right_val,
                    "&" => left_val & right_val,
                    "|" => left_val | right_val,
                    "^" => left_val ^ right_val,
                    _ => {
                        return Err(format!(
                            "Unsupported binary operator in const expression: {}",
                            op
                        ));
                    }
                }
            }
            AstNode::UnaryOp { op, expr } => {
                let val = self.eval_const_expr(expr)?;
                match op.as_str() {
                    "-" => -val,
                    "!" => !val,
                    "~" => !val,
                    _ => {
                        return Err(format!(
                            "Unsupported unary operator in const expression: {}",
                            op
                        ));
                    }
                }
            }
            AstNode::Call {
                receiver,
                method,
                args,
                ..
            } => {
                // Check if it's a const function call
                if receiver.is_some() {
                    return Err("Method calls not supported in const expressions".to_string());
                }

                // For now, only support simple built-in const functions
                match method.as_str() {
                    "min" => {
                        if args.len() != 2 {
                            return Err("min() requires 2 arguments".to_string());
                        }
                        let a = self.eval_const_expr(&args[0])?;
                        let b = self.eval_const_expr(&args[1])?;
                        a.min(b)
                    }
                    "max" => {
                        if args.len() != 2 {
                            return Err("max() requires 2 arguments".to_string());
                        }
                        let a = self.eval_const_expr(&args[0])?;
                        let b = self.eval_const_expr(&args[1])?;
                        a.max(b)
                    }
                    "abs" => {
                        if args.len() != 1 {
                            return Err("abs() requires 1 argument".to_string());
                        }
                        let a = self.eval_const_expr(&args[0])?;
                        a.abs()
                    }
                    _ => return Err(format!("Unsupported const function: {}", method)),
                }
            }
            AstNode::ConstDef { value, comptime_, .. } => {
                // Evaluate the constant's value
                self.eval_const_expr(value)?
            }
            _ => {
                return Err(format!(
                    "Unsupported expression in const context: {:?}",
                    expr
                ));
            }
        };

        // Cache the result
        self.cache.insert(expr.clone(), value);
        Ok(value)
    }

    /// Check if a function can be evaluated at compile time
    pub fn is_const_function(&self, func: &AstNode) -> bool {
        match func {
            AstNode::FuncDef { const_, .. } => *const_,
            _ => false,
        }
    }

    /// Try to evaluate a const function call at compile time
    pub fn try_eval_const_call(
        &mut self,
        func: &AstNode,
        args: &[AstNode],
    ) -> Result<Option<i64>, String> {
        match func {
            AstNode::FuncDef {
                const_,
                comptime_,
                body,
                ret_expr,
                ..
            } => {
                if !*const_ && !*comptime_ {
                    return Ok(None);
                }

                // For now, only evaluate simple const functions with literal returns
                if let Some(expr) = ret_expr {
                    // Evaluate the return expression with argument substitution
                    // This is a simplified implementation
                    self.eval_const_expr(expr).map(Some)
                } else if !body.is_empty() {
                    // Try to evaluate the last expression in the body
                    if let Some(AstNode::ExprStmt { expr }) = body.last() {
                        self.eval_const_expr(expr).map(Some)
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }
}

/// Evaluate constant definitions at compile time
pub fn evaluate_constants(asts: &[AstNode]) -> Result<Vec<AstNode>, String> {
    let mut evaluator = ConstEvaluator::new();
    let mut result = Vec::new();

    for ast in asts {
        match ast {
            AstNode::ConstDef {
                name,
                ty,
                value,
                pub_,
                comptime_,
            } => {
                // Try to evaluate the constant
                match evaluator.eval_const_expr(value) {
                    Ok(val) => {
                        // Replace with a literal if evaluation succeeds
                        result.push(AstNode::ConstDef {
                            name: name.clone(),
                            ty: ty.clone(),
                            value: Box::new(AstNode::Lit(val)),
                            pub_: *pub_,
                            comptime_: *comptime_,
                        });
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
