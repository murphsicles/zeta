//! CTFE evaluation context.
//! Tracks compile-time known values and functions.

use crate::frontend::ast::AstNode;
use std::collections::HashMap;

/// Context for compile-time evaluation
#[derive(Debug, Clone, Default)]
pub struct ConstContext {
    functions: HashMap<String, AstNode>,
    variables: HashMap<String, crate::middle::types::ConstValue>,
}

impl ConstContext {
    /// Create a new empty context
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    /// Register a function (const or comptime) in the context
    pub fn register_function(&mut self, name: String, ast: AstNode) {
        self.functions.insert(name, ast);
    }

    /// Look up a function by name
    pub fn get_function(&self, name: &str) -> Option<&AstNode> {
        self.functions.get(name)
    }

    /// Set a variable's compile-time value
    pub fn set_variable(&mut self, name: String, value: crate::middle::types::ConstValue) {
        self.variables.insert(name, value);
    }

    /// Get a variable's compile-time value
    pub fn get_variable(&self, name: &str) -> Option<&crate::middle::types::ConstValue> {
        self.variables.get(name)
    }
}