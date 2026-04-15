//! CTFE error types.
//! Defines `CtfeError` and `CtfeResult` for compile-time evaluation errors.

use std::fmt;

/// CTFE evaluation error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CtfeError {
    /// Expression is not constant
    NotConstant,
    /// Division by zero
    DivisionByZero,
    /// Arithmetic overflow
    Overflow,
    /// Unknown variable
    UnknownVariable(String),
    /// Type mismatch in operation
    TypeMismatch(String),
    /// Unsupported operation at compile time
    UnsupportedOperation(String),
    /// Function call failed
    FunctionCallError(String),
}

impl fmt::Display for CtfeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CtfeError::NotConstant => write!(f, "expression is not constant"),
            CtfeError::DivisionByZero => write!(f, "division by zero"),
            CtfeError::Overflow => write!(f, "arithmetic overflow"),
            CtfeError::UnknownVariable(name) => write!(f, "unknown variable: {}", name),
            CtfeError::TypeMismatch(msg) => write!(f, "type mismatch: {}", msg),
            CtfeError::UnsupportedOperation(msg) => write!(f, "unsupported operation: {}", msg),
            CtfeError::FunctionCallError(msg) => write!(f, "function call error: {}", msg),
        }
    }
}

impl std::error::Error for CtfeError {}

/// Result type for CTFE operations
pub type CtfeResult<T> = Result<T, CtfeError>;