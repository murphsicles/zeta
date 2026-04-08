//! Hygiene system for macro expansion
//!
//! Prevents identifier capture and ensures macro expansions don't
//! accidentally bind to identifiers in the surrounding scope.

use std::collections::HashMap;

/// Context for hygienic macro expansion
#[derive(Debug, Default)]
pub struct HygieneContext {
    /// Map from original identifier to hygienic identifier
    renames: HashMap<String, String>,
    /// Counter for generating unique identifiers
    counter: u64,
}

impl HygieneContext {
    /// Create a new hygiene context
    pub fn new() -> Self {
        Self {
            renames: HashMap::new(),
            counter: 0,
        }
    }
    
    /// Make an identifier hygienic
    pub fn hygienic_ident(&mut self, ident: &str) -> String {
        if let Some(renamed) = self.renames.get(ident) {
            renamed.clone()
        } else {
            let new_ident = format!("{}__{}", ident, self.counter);
            self.counter += 1;
            self.renames.insert(ident.to_string(), new_ident.clone());
            new_ident
        }
    }
    
    /// Apply hygiene to a list of identifiers
    pub fn hygienic_idents(&mut self, idents: &[String]) -> Vec<String> {
        idents.iter()
            .map(|ident| self.hygienic_ident(ident))
            .collect()
    }
    
    /// Check if an identifier has been made hygienic
    pub fn is_hygienic(&self, ident: &str) -> bool {
        self.renames.contains_key(ident)
    }
    
    /// Get the hygienic version of an identifier
    pub fn get_hygienic(&self, ident: &str) -> Option<&String> {
        self.renames.get(ident)
    }
    
    /// Clear all hygiene mappings
    pub fn clear(&mut self) {
        self.renames.clear();
        self.counter = 0;
    }
}