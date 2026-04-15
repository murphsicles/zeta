//! CTFE value representation.
//! Defines the `ConstValue` enum for compile-time values.

/// Constant value for compile-time evaluation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstValue {
    /// Signed 64-bit integer
    Int(i64),
    /// Unsigned 64-bit integer
    UInt(u64),
    /// Boolean value
    Bool(bool),
    /// Array of constant values
    Array(Vec<ConstValue>),
}

impl ConstValue {
    /// Convert to i64 if possible
    pub fn as_int(&self) -> Option<i64> {
        match self {
            ConstValue::Int(n) => Some(*n),
            ConstValue::UInt(n) => Some(*n as i64),
            _ => None,
        }
    }

    /// Convert to u64 if possible
    pub fn as_uint(&self) -> Option<u64> {
        match self {
            ConstValue::UInt(n) => Some(*n),
            ConstValue::Int(n) => Some(*n as u64),
            _ => None,
        }
    }

    /// Convert to bool if possible
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Check if value is truthy (non-zero integer, true boolean)
    pub fn is_truthy(&self) -> bool {
        match self {
            ConstValue::Int(n) => *n != 0,
            ConstValue::UInt(n) => *n != 0,
            ConstValue::Bool(b) => *b,
            ConstValue::Array(arr) => !arr.is_empty(),
        }
    }
}