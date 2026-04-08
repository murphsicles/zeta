//! Hygienic macro system to prevent identifier capture.

use std::collections::HashMap;

/// Context for hygienic macro expansion
#[derive(Debug, Default)]
pub struct HygieneContext {
    /// Map from original identifiers to fresh identifiers
    fresh_ids: HashMap<String, String>,
    /// Counter for generating fresh identifiers
    counter: u64,
}

impl HygieneContext {
    /// Create a new hygiene context
    pub fn new() -> Self {
        Self {
            fresh_ids: HashMap::new(),
            counter: 0,
        }
    }

    /// Generate a fresh identifier for hygienic expansion
    pub fn fresh_ident(&mut self, original: &str) -> String {
        if let Some(fresh) = self.fresh_ids.get(original) {
            return fresh.clone();
        }

        let fresh = format!("{}__{}", original, self.counter);
        self.counter += 1;
        self.fresh_ids.insert(original.to_string(), fresh.clone());
        fresh
    }

    /// Reset the hygiene context
    pub fn reset(&mut self) {
        self.fresh_ids.clear();
        self.counter = 0;
    }
}