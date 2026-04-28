//! Error types for CTFE (Compile-Time Function Evaluation)

use std::fmt;

/// CTFE error type
#[derive(Debug, Clone, PartialEq)]
pub enum CtfeError {
    /// Expression cannot be evaluated at compile time
    NotConstant,
    /// Division by zero in constant expression
    DivisionByZero,
    /// Arithmetic overflow in constant expression
    Overflow,
    /// Unknown variable name
    UnknownVariable(String),
    /// Unsupported expression type in const context
    UnsupportedExpression(String),
    /// Invalid operation for the given types
    InvalidOperation {
        op: String,
        left_type: String,
        right_type: Option<String>,
    },
    /// Undefined variable
    UndefinedVariable(String),
    /// Argument count mismatch
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
    },
    /// Type mismatch
    TypeMismatch {
        expected: String,
        found: String,
    },
    /// Function not found
    FunctionNotFound(String),
    /// Function call failed
    FunctionCallFailed(String),
    /// Unsupported operation
    UnsupportedOperation(String),
    /// Scope management error
    ScopeError(String),
    /// Loop exceeded maximum allowed iterations
    LoopTooManyIterations,
    /// Index out of bounds in array access
    IndexOutOfBounds {
        index: usize,
        length: usize,
    },
}

impl fmt::Display for CtfeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CtfeError::NotConstant => write!(f, "expression is not constant"),
            CtfeError::DivisionByZero => write!(f, "division by zero"),
            CtfeError::Overflow => write!(f, "arithmetic overflow"),
            CtfeError::UnknownVariable(name) => write!(f, "unknown variable: {}", name),
            CtfeError::UnsupportedExpression(msg) => write!(f, "unsupported expression: {}", msg),
            CtfeError::InvalidOperation { op, left_type, right_type } => {
                if let Some(right_type) = right_type {
                    write!(f, "invalid operation '{}' for types {} and {}", op, left_type, right_type)
                } else {
                    write!(f, "invalid operation '{}' for type {}", op, left_type)
                }
            }
            CtfeError::UndefinedVariable(name) => write!(f, "undefined variable: {}", name),
            CtfeError::ArgumentCountMismatch { expected, found } => {
                write!(f, "expected {} arguments, found {}", expected, found)
            }
            CtfeError::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected {}, found {}", expected, found)
            }
            CtfeError::FunctionNotFound(name) => write!(f, "function not found: {}", name),
            CtfeError::FunctionCallFailed(msg) => write!(f, "function call failed: {}", msg),
            CtfeError::UnsupportedOperation(msg) => write!(f, "unsupported operation: {}", msg),
            CtfeError::ScopeError(msg) => write!(f, "scope error: {}", msg),
            CtfeError::LoopTooManyIterations => write!(f, "loop exceeded maximum allowed iterations"),
            CtfeError::IndexOutOfBounds { index, length } => {
                write!(f, "index out of bounds: index {}, length {}", index, length)
            }
        }
    }
}

impl std::error::Error for CtfeError {}

/// Result type for CTFE operations
pub type CtfeResult<T> = Result<T, CtfeError>;