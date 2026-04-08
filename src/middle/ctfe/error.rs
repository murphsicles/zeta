//! CTFE error types
//!
//! This module defines error types for compile-time evaluation.

use std::fmt;

/// CTFE error type
#[derive(Debug, Clone)]
pub enum CtfeError {
    /// Expression is not constant
    NotConstant,
    /// Division by zero
    DivisionByZero,
    /// Arithmetic overflow
    Overflow,
    /// Unknown variable
    UnknownVariable(String),
    /// Unknown function
    UnknownFunction(String),
    /// Type mismatch
    TypeMismatch,
    /// Recursion depth exceeded
    RecursionDepthExceeded,
    /// Unsupported operation
    UnsupportedOperation(String),
}

impl fmt::Display for CtfeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CtfeError::NotConstant => write!(f, "expression is not constant"),
            CtfeError::DivisionByZero => write!(f, "division by zero"),
            CtfeError::Overflow => write!(f, "arithmetic overflow"),
            CtfeError::UnknownVariable(name) => write!(f, "unknown variable: {}", name),
            CtfeError::UnknownFunction(name) => write!(f, "unknown function: {}", name),
            CtfeError::TypeMismatch => write!(f, "type mismatch"),
            CtfeError::RecursionDepthExceeded => write!(f, "recursion depth exceeded"),
            CtfeError::UnsupportedOperation(op) => write!(f, "unsupported operation: {}", op),
        }
    }
}

/// Result type for CTFE operations
pub type CtfeResult<T> = Result<T, CtfeError>;