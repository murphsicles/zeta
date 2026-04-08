//! Macro registry for storing and looking up macro definitions.

use std::collections::HashMap;
use crate::frontend::ast::AstNode;

/// A macro definition with its pattern and template
#[derive(Debug, Clone)]
pub struct MacroDef {
    /// Macro name
    pub name: String,
    /// Pattern to match
    pub pattern: AstNode,
    /// Template to expand to
    pub template: AstNode,
    /// Whether this macro is hygienic
    pub hygienic: bool,
}

/// Registry for macro definitions
#[derive(Debug, Default)]
pub struct MacroRegistry {
    /// Map from macro name to macro definitions
    macros: HashMap<String, Vec<MacroDef>>,
}

impl MacroRegistry {
    /// Create a new empty macro registry
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
        }
    }

    /// Register a new macro definition
    pub fn register(&mut self, def: MacroDef) {
        self.macros
            .entry(def.name.clone())
            .or_insert_with(Vec::new)
            .push(def);
    }

    /// Look up macro definitions by name
    pub fn lookup(&self, name: &str) -> Option<&[MacroDef]> {
        self.macros.get(name).map(|v| v.as_slice())
    }

    /// Check if a macro with the given name exists
    pub fn contains(&self, name: &str) -> bool {
        self.macros.contains_key(name)
    }

    /// Get all registered macro names
    pub fn names(&self) -> Vec<String> {
        self.macros.keys().cloned().collect()
    }
}