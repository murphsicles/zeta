//! Stub for std::collections::HashMap
//! Minimal implementation for compilation

use std::hash::Hash;

/// Stub HashMap type
pub struct HashMap<K, V> {
    // Empty stub - actual implementation would be in host runtime
}

impl<K, V> HashMap<K, V> {
    /// Create a new empty HashMap
    pub fn new() -> Self {
        HashMap {}
    }
    
    /// Insert a key-value pair
    pub fn insert(&mut self, _key: K, _value: V) -> Option<V> {
        None // Stub
    }
    
    /// Get a reference to the value associated with the key
    pub fn get(&self, _key: &K) -> Option<&V> {
        None // Stub
    }
}

impl<K, V> Default for HashMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}