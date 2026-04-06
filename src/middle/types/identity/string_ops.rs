//! String operations with identity capabilities
//! 
//! This module extends string operations with identity semantics,
//! allowing strings to have associated capabilities (Read, Write, Execute, Owned).

use super::{CapabilityLevel, IdentityType};
use crate::middle::types::Type;

/// String with identity capabilities
#[derive(Debug, Clone, PartialEq)]
pub struct StringWithIdentity {
    value: String,
    identity: IdentityType,
}

impl StringWithIdentity {
    /// Create a new string with the given capabilities
    pub fn new(value: String, caps: Vec<CapabilityLevel>) -> Self {
        let identity = IdentityType::with_value(value.clone(), caps);
        Self { value, identity }
    }
    
    /// Get the string value (requires Read capability)
    pub fn get(&self) -> &str {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for get()");
        }
        &self.value
    }
    
    /// Get mutable access to the string (requires Write capability)
    pub fn get_mut(&mut self) -> &mut String {
        if !self.identity.has_capability(CapabilityLevel::Write) {
            panic!("String requires Write capability for get_mut()");
        }
        &mut self.value
    }
    
    /// Check if the string has a specific capability
    pub fn has_capability(&self, cap: CapabilityLevel) -> bool {
        self.identity.has_capability(cap)
    }
    
    /// Get all capabilities
    pub fn capabilities(&self) -> &[CapabilityLevel] {
        self.identity.capabilities()
    }
    
    /// Upgrade capabilities (add new capabilities)
    pub fn upgrade(mut self, new_caps: Vec<CapabilityLevel>) -> Self {
        self.identity = self.identity.upgrade(new_caps);
        self
    }
    
    /// Downgrade capabilities (remove capabilities)
    pub fn downgrade(mut self, remove_caps: Vec<CapabilityLevel>) -> Self {
        self.identity = self.identity.downgrade(remove_caps);
        self
    }
    
    /// Clone the string (requires Owned capability)
    pub fn clone_string(&self) -> Self {
        if !self.identity.has_capability(CapabilityLevel::Owned) {
            panic!("String requires Owned capability for clone_string()");
        }
        Self {
            value: self.value.clone(),
            identity: self.identity.clone(),
        }
    }
    
    /// Get length of the string (requires Read capability)
    pub fn len(&self) -> usize {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for len()");
        }
        self.value.len()
    }
    
    /// Check if string is empty (requires Read capability)
    pub fn is_empty(&self) -> bool {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for is_empty()");
        }
        self.value.is_empty()
    }
    
    /// Append to the string (requires Write capability)
    pub fn append(&mut self, other: &str) {
        if !self.identity.has_capability(CapabilityLevel::Write) {
            panic!("String requires Write capability for append()");
        }
        self.value.push_str(other);
    }
    
    /// Convert to uppercase (requires Read and Write capabilities)
    pub fn to_uppercase(&mut self) {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for to_uppercase()");
        }
        if !self.identity.has_capability(CapabilityLevel::Write) {
            panic!("String requires Write capability for to_uppercase()");
        }
        self.value = self.value.to_uppercase();
    }
    
    /// Convert to lowercase (requires Read and Write capabilities)
    pub fn to_lowercase(&mut self) {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for to_lowercase()");
        }
        if !self.identity.has_capability(CapabilityLevel::Write) {
            panic!("String requires Write capability for to_lowercase()");
        }
        self.value = self.value.to_lowercase();
    }
    
    /// Trim whitespace (requires Read and Write capabilities)
    pub fn trim(&mut self) {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for trim()");
        }
        if !self.identity.has_capability(CapabilityLevel::Write) {
            panic!("String requires Write capability for trim()");
        }
        self.value = self.value.trim().to_string();
    }
    
    /// Check if string starts with a prefix (requires Read capability)
    pub fn starts_with(&self, prefix: &str) -> bool {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for starts_with()");
        }
        self.value.starts_with(prefix)
    }
    
    /// Check if string ends with a suffix (requires Read capability)
    pub fn ends_with(&self, suffix: &str) -> bool {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for ends_with()");
        }
        self.value.ends_with(suffix)
    }
    
    /// Check if string contains a substring (requires Read capability)
    pub fn contains(&self, needle: &str) -> bool {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for contains()");
        }
        self.value.contains(needle)
    }
    
    /// Replace all occurrences of a pattern (requires Read and Write capabilities)
    pub fn replace(&mut self, from: &str, to: &str) {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for replace()");
        }
        if !self.identity.has_capability(CapabilityLevel::Write) {
            panic!("String requires Write capability for replace()");
        }
        self.value = self.value.replace(from, to);
    }
    
    /// Get a substring (requires Read capability)
    /// Returns a new string with the same capabilities as the original
    pub fn substring(&self, start: usize, end: usize) -> Self {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for substring()");
        }
        if start > end || end > self.value.len() {
            panic!("Invalid substring range");
        }
        let substring = self.value[start..end].to_string();
        Self {
            value: substring,
            identity: self.identity.clone(),
        }
    }
    
    /// Concatenate with another string (requires Read capability on both)
    /// The resulting string has the intersection of capabilities from both strings
    pub fn concat(&self, other: &Self) -> Self {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("First string requires Read capability for concat()");
        }
        if !other.identity.has_capability(CapabilityLevel::Read) {
            panic!("Second string requires Read capability for concat()");
        }
        
        // Get intersection of capabilities
        let self_caps: std::collections::HashSet<_> = self.identity.capabilities().iter().collect();
        let other_caps: std::collections::HashSet<_> = other.identity.capabilities().iter().collect();
        let intersection: Vec<CapabilityLevel> = self_caps
            .intersection(&other_caps)
            .cloned()
            .copied()
            .collect();
        
        let concatenated = format!("{}{}", self.value, other.value);
        Self::new(concatenated, intersection)
    }
    
    /// Split the string by a delimiter (requires Read capability)
    /// Returns a vector of strings with the same capabilities as the original
    pub fn split(&self, delimiter: &str) -> Vec<Self> {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for split()");
        }
        
        self.value
            .split(delimiter)
            .map(|part| {
                Self {
                    value: part.to_string(),
                    identity: self.identity.clone(),
                }
            })
            .collect()
    }
    
    /// Find the index of a substring (requires Read capability)
    pub fn find(&self, needle: &str) -> Option<usize> {
        if !self.identity.has_capability(CapabilityLevel::Read) {
            panic!("String requires Read capability for find()");
        }
        self.value.find(needle)
    }
}

/// Create a string with read-only capabilities
pub fn read_only_string(value: String) -> StringWithIdentity {
    StringWithIdentity::new(value, vec![CapabilityLevel::Read])
}

/// Create a string with read-write capabilities
pub fn read_write_string(value: String) -> StringWithIdentity {
    StringWithIdentity::new(value, vec![CapabilityLevel::Read, CapabilityLevel::Write])
}

/// Create a string with full capabilities (Read, Write, Owned)
pub fn owned_string(value: String) -> StringWithIdentity {
    StringWithIdentity::new(value, vec![
        CapabilityLevel::Read,
        CapabilityLevel::Write,
        CapabilityLevel::Owned,
    ])
}

/// Infer required capabilities for a string operation
pub fn infer_string_op_capabilities(op: &str) -> Vec<CapabilityLevel> {
    match op {
        "len" | "is_empty" | "starts_with" | "ends_with" | "contains" | "find" => {
            vec![CapabilityLevel::Read]
        }
        "append" => vec![CapabilityLevel::Write],
        "clone_string" => vec![CapabilityLevel::Owned],
        "to_uppercase" | "to_lowercase" | "trim" | "replace" => {
            vec![CapabilityLevel::Read, CapabilityLevel::Write]
        }
        "substring" | "split" => vec![CapabilityLevel::Read],
        "concat" => vec![CapabilityLevel::Read], // Both strings need Read capability
        _ => vec![],
    }
}

/// Check if a string has the required capabilities for an operation
pub fn check_string_op_capabilities(
    string: &StringWithIdentity,
    op: &str,
) -> Result<(), String> {
    let required = infer_string_op_capabilities(op);
    
    for cap in required {
        if !string.has_capability(cap) {
            return Err(format!(
                "String operation '{}' requires {:?} capability",
                op, cap
            ));
        }
    }
    
    Ok(())
}