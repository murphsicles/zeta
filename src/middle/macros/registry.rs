//! Macro registry for storing and retrieving macro definitions

use std::collections::HashMap;
use crate::frontend::ast::AstNode;

/// Represents a macro definition
#[derive(Debug, Clone)]
pub struct MacroDef {
    /// Macro name
    pub name: String,
    /// Macro parameters
    pub params: Vec<String>,
    /// Macro body (template)
    pub body: AstNode,
    /// Whether this is a hygienic macro
    pub hygienic: bool,
}

/// Registry for macro definitions
#[derive(Debug, Default)]
pub struct MacroRegistry {
    /// Map from macro name to definition
    macros: HashMap<String, MacroDef>,
}

impl MacroRegistry {
    /// Create a new empty macro registry
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
        }
    }
    
    /// Register a new macro
    pub fn register(&mut self, name: String, params: Vec<String>, body: AstNode, hygienic: bool) {
        let def = MacroDef {
            name: name.clone(),
            params,
            body,
            hygienic,
        };
        self.macros.insert(name, def);
    }
    
    /// Get a macro definition by name
    pub fn get(&self, name: &str) -> Option<&MacroDef> {
        self.macros.get(name)
    }
    
    /// Check if a macro is registered
    pub fn contains(&self, name: &str) -> bool {
        self.macros.contains_key(name)
    }
    
    /// Remove a macro definition
    pub fn remove(&mut self, name: &str) -> Option<MacroDef> {
        self.macros.remove(name)
    }
    
    /// Get all registered macro names
    pub fn names(&self) -> Vec<String> {
        self.macros.keys().cloned().collect()
    }
    
    /// Clear all macro definitions
    pub fn clear(&mut self) {
        self.macros.clear();
    }
}