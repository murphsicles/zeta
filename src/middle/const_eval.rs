// src/middle/const_eval.rs
//! Compile-Time Function Evaluation (CTFE) for Zeta v0.5.0
//!
//! Evaluates const functions at compile time.

use crate::frontend::ast::AstNode;
use std::collections::HashMap;
use std::fmt;

/// Value that can be computed at compile time
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstValue {
    /// Integer literal
    Int(i64),
    /// Array of compile-time values
    Array(Vec<ConstValue>),
    /// Boolean value
    Bool(bool),
}

impl ConstValue {
    /// Convert to i64 if this is an integer
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            ConstValue::Int(n) => Some(*n),
            _ => None,
        }
    }
    
    /// Convert to bool if this is a boolean
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
    
    /// Get array length if this is an array
    pub fn array_len(&self) -> Option<usize> {
        match self {
            ConstValue::Array(arr) => Some(arr.len()),
            _ => None,
        }
    }
}

impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstValue::Int(n) => write!(f, "{}", n),
            ConstValue::Bool(b) => write!(f, "{}", b),
            ConstValue::Array(arr) => {
                write!(f, "[")?;
                for (i, elem) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
        }
    }
}

/// Constant evaluator for compile-time evaluation
pub struct ConstEvaluator {
    /// Cache of evaluated constants
    cache: HashMap<AstNode, ConstValue>,
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

    /// Evaluate a const expression to a ConstValue
    pub fn eval_const_expr(&mut self, expr: &AstNode) -> Result<ConstValue, String> {
        // Check cache first
        if let Some(value) = self.cache.get(expr) {
            return Ok(value.clone());
        }

        let value = match expr {
            AstNode::Lit(n) => ConstValue::Int(*n),
            AstNode::BinaryOp { op, left, right } => {
                let left_val = self.eval_const_expr(left)?;
                let right_val = self.eval_const_expr(right)?;

                // For now, only support integer operations
                let left_int = left_val.as_i64().ok_or_else(|| {
                    format!("Left operand must be integer for binary operator {}, got {:?}", op, left_val)
                })?;
                let right_int = right_val.as_i64().ok_or_else(|| {
                    format!("Right operand must be integer for binary operator {}, got {:?}", op, right_val)
                })?;

                match op.as_str() {
                    "+" => ConstValue::Int(left_int + right_int),
                    "-" => ConstValue::Int(left_int - right_int),
                    "*" => ConstValue::Int(left_int * right_int),
                    "/" => {
                        if right_int == 0 {
                            return Err("Division by zero in const expression".to_string());
                        }
                        ConstValue::Int(left_int / right_int)
                    }
                    "%" => {
                        if right_int == 0 {
                            return Err("Modulo by zero in const expression".to_string());
                        }
                        ConstValue::Int(left_int % right_int)
                    }
                    "<<" => ConstValue::Int(left_int << right_int),
                    ">>" => ConstValue::Int(left_int >> right_int),
                    "&" => ConstValue::Int(left_int & right_int),
                    "|" => ConstValue::Int(left_int | right_int),
                    "^" => ConstValue::Int(left_int ^ right_int),
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
                let int_val = val.as_i64().ok_or_else(|| {
                    format!("Operand must be integer for unary operator {}, got {:?}", op, val)
                })?;
                
                match op.as_str() {
                    "-" => ConstValue::Int(-int_val),
                    "!" => ConstValue::Int(!int_val),
                    "~" => ConstValue::Int(!int_val),
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
                        let a = self.eval_const_expr(&args[0])?.as_i64()
                            .ok_or_else(|| "min() requires integer arguments".to_string())?;
                        let b = self.eval_const_expr(&args[1])?.as_i64()
                            .ok_or_else(|| "min() requires integer arguments".to_string())?;
                        ConstValue::Int(a.min(b))
                    }
                    "max" => {
                        if args.len() != 2 {
                            return Err("max() requires 2 arguments".to_string());
                        }
                        let a = self.eval_const_expr(&args[0])?.as_i64()
                            .ok_or_else(|| "max() requires integer arguments".to_string())?;
                        let b = self.eval_const_expr(&args[1])?.as_i64()
                            .ok_or_else(|| "max() requires integer arguments".to_string())?;
                        ConstValue::Int(a.max(b))
                    }
                    "abs" => {
                        if args.len() != 1 {
                            return Err("abs() requires 1 argument".to_string());
                        }
                        let a = self.eval_const_expr(&args[0])?.as_i64()
                            .ok_or_else(|| "abs() requires integer argument".to_string())?;
                        ConstValue::Int(a.abs())
                    }
                    _ => return Err(format!("Unsupported const function: {}", method)),
                }
            }
            AstNode::ConstDef { value, comptime_, .. } => {
                // Evaluate the constant's value
                self.eval_const_expr(value)?
            }
            AstNode::ArrayLit(elements) => {
                // Evaluate all array elements
                let mut const_elements = Vec::new();
                for elem in elements {
                    const_elements.push(self.eval_const_expr(elem)?);
                }
                ConstValue::Array(const_elements)
            }
            AstNode::Bool(b) => ConstValue::Bool(*b),
            _ => {
                return Err(format!(
                    "Unsupported expression in const context: {:?}",
                    expr
                ));
            }
        };

        // Cache the result
        self.cache.insert(expr.clone(), value.clone());
        Ok(value)
    }

    /// Check if a function can be evaluated at compile time
    pub fn is_const_function(&self, func: &AstNode) -> bool {
        match func {
            AstNode::FuncDef { const_, comptime_, .. } => *const_ || *comptime_,
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
                    self.eval_const_expr(expr).map(|v| v.as_i64())
                } else if !body.is_empty() {
                    // Try to evaluate the last expression in the body
                    if let Some(AstNode::ExprStmt { expr }) = body.last() {
                        self.eval_const_expr(expr).map(|v| v.as_i64())
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
                    Ok(ConstValue::Int(val)) => {
                        // Replace with a literal if evaluation succeeds
                        result.push(AstNode::ConstDef {
                            name: name.clone(),
                            ty: ty.clone(),
                            value: Box::new(AstNode::Lit(val)),
                            pub_: *pub_,
                            comptime_: *comptime_,
                        });
                    }
                    Ok(_) => {
                        // Non-integer constant value - keep as-is for now
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
