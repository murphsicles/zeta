//! Simple CTFE evaluator for Zeta
//!
//! This is a minimal working CTFE implementation that can evaluate
//! basic constant expressions.

use crate::frontend::ast::AstNode;
use super::error::{CtfeError, CtfeResult};
use super::value::ConstValue;

/// Simple constant evaluator
#[derive(Debug, Default)]
pub struct SimpleConstEvaluator;

impl SimpleConstEvaluator {
    /// Create a new simple evaluator
    pub fn new() -> Self {
        Self
    }
    
    /// Evaluate a constant expression
    pub fn eval_const_expr(&self, expr: &AstNode) -> CtfeResult<ConstValue> {
        match expr {
            // Literals
            AstNode::Lit(n) => Ok(ConstValue::Int(*n)),
            AstNode::Bool(b) => Ok(ConstValue::Bool(*b)),
            
            // Binary operations
            AstNode::BinaryOp { op, left, right } => {
                let left_val = self.eval_const_expr(left)?;
                let right_val = self.eval_const_expr(right)?;
                
                match (left_val, right_val) {
                    (ConstValue::Int(a), ConstValue::Int(b)) => {
                        match op.as_str() {
                            "+" => a.checked_add(b)
                                .map(ConstValue::Int)
                                .ok_or(CtfeError::Overflow),
                            "-" => a.checked_sub(b)
                                .map(ConstValue::Int)
                                .ok_or(CtfeError::Overflow),
                            "*" => a.checked_mul(b)
                                .map(ConstValue::Int)
                                .ok_or(CtfeError::Overflow),
                            "/" => {
                                if b == 0 {
                                    Err(CtfeError::DivisionByZero)
                                } else {
                                    a.checked_div(b)
                                        .map(ConstValue::Int)
                                        .ok_or(CtfeError::Overflow)
                                }
                            }
                            "%" => {
                                if b == 0 {
                                    Err(CtfeError::DivisionByZero)
                                } else {
                                    Ok(ConstValue::Int(a % b))
                                }
                            }
                            "==" => Ok(ConstValue::Bool(a == b)),
                            "!=" => Ok(ConstValue::Bool(a != b)),
                            "<" => Ok(ConstValue::Bool(a < b)),
                            "<=" => Ok(ConstValue::Bool(a <= b)),
                            ">" => Ok(ConstValue::Bool(a > b)),
                            ">=" => Ok(ConstValue::Bool(a >= b)),
                            "&&" => Ok(ConstValue::Bool(a != 0 && b != 0)),
                            "||" => Ok(ConstValue::Bool(a != 0 || b != 0)),
                            _ => Err(CtfeError::UnsupportedOperation(op.clone())),
                        }
                    }
                    (ConstValue::Bool(a), ConstValue::Bool(b)) => {
                        match op.as_str() {
                            "==" => Ok(ConstValue::Bool(a == b)),
                            "!=" => Ok(ConstValue::Bool(a != b)),
                            "&&" => Ok(ConstValue::Bool(a && b)),
                            "||" => Ok(ConstValue::Bool(a || b)),
                            _ => Err(CtfeError::UnsupportedOperation(op.clone())),
                        }
                    }
                    _ => Err(CtfeError::TypeMismatch),
                }
            }
            
            // Unary operations
            AstNode::UnaryOp { op, expr } => {
                let val = self.eval_const_expr(expr)?;
                
                match (op.as_str(), val) {
                    ("-", ConstValue::Int(n)) => {
                        n.checked_neg()
                            .map(ConstValue::Int)
                            .ok_or(CtfeError::Overflow)
                    }
                    ("!", ConstValue::Bool(b)) => Ok(ConstValue::Bool(!b)),
                    ("!", ConstValue::Int(n)) => Ok(ConstValue::Bool(n == 0)),
                    _ => Err(CtfeError::UnsupportedOperation(op.clone())),
                }
            }
            
            // Unsupported expressions
            _ => Err(CtfeError::NotConstant),
        }
    }
    
    /// Evaluate a program (list of AST nodes)
    pub fn evaluate_program(&self, asts: &[AstNode]) -> CtfeResult<Vec<AstNode>> {
        let mut result = Vec::new();
        
        for ast in asts {
            match ast {
                AstNode::ConstDef { name, ty, value, attrs, pub_, comptime_ } => {
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
                            // Non-simple constant value - keep as-is
                            result.push(ast.clone());
                        }
                        Err(_) => {
                            // Keep as-is if evaluation fails
                            result.push(ast.clone());
                        }
                    }
                }
                _ => result.push(ast.clone()),
            }
        }
        
        Ok(result)
    }
}