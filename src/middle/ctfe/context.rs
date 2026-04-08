//! Const evaluation context
//!
//! This module defines the context for compile-time evaluation,
//! including registered functions and variables.

use std::collections::HashMap;
use crate::frontend::ast::AstNode;

/// Context for compile-time evaluation
#[derive(Debug, Clone, Default)]
pub struct ConstContext {
    /// Registered const/comptime functions
    functions: HashMap<String, AstNode>,
    /// Current variable bindings
    variables: HashMap<String, i64>,
}

impl ConstContext {
    /// Create a new empty context
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }
    
    /// Register a const/comptime function
    pub fn register_function(&mut self, name: String, node: AstNode) {
        self.functions.insert(name, node);
    }
    
    /// Get a registered function
    pub fn get_function(&self, name: &str) -> Option<&AstNode> {
        self.functions.get(name)
    }
    
    /// Set a variable value
    pub fn set_variable(&mut self, name: String, value: i64) {
        self.variables.insert(name, value);
    }
    
    /// Get a variable value
    pub fn get_variable(&self, name: &str) -> Option<i64> {
        self.variables.get(name).copied()
    }
    
    /// Check if a function is registered
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }
}