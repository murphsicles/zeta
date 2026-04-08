//! Const value representation
//!
//! This module defines the representation of values during
//! compile-time evaluation.

#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    /// Integer value
    Int(i64),
    /// Unsigned integer value
    UInt(u64),
    /// Boolean value
    Bool(bool),
    /// Array value
    Array(Vec<ConstValue>),
}

impl ConstValue {
    /// Convert to i64 if possible
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            ConstValue::Int(n) => Some(*n),
            ConstValue::UInt(n) => Some(*n as i64),
            ConstValue::Bool(b) => Some(if *b { 1 } else { 0 }),
            ConstValue::Array(_) => None,
        }
    }
    
    /// Convert to u64 if possible
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            ConstValue::Int(n) => Some(*n as u64),
            ConstValue::UInt(n) => Some(*n),
            ConstValue::Bool(b) => Some(if *b { 1 } else { 0 }),
            ConstValue::Array(_) => None,
        }
    }
    
    /// Convert to bool if possible
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(b) => Some(*b),
            ConstValue::Int(n) => Some(*n != 0),
            ConstValue::UInt(n) => Some(*n != 0),
            ConstValue::Array(_) => None,
        }
    }
    
    /// Check if value is truthy
    pub fn is_truthy(&self) -> bool {
        match self {
            ConstValue::Bool(b) => *b,
            ConstValue::Int(n) => *n != 0,
            ConstValue::UInt(n) => *n != 0,
            ConstValue::Array(arr) => !arr.is_empty(),
        }
    }
}