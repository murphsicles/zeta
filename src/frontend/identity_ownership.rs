// src/frontend/identity_ownership.rs
//! # Identity-Aware Ownership System
//!
//! Extends the ownership system with identity capabilities for Phase 4.3.
//! Integrates identity types with ownership tracking and borrowing rules.

use crate::middle::resolver::resolver::Type;
use crate::middle::types::identity::{IdentityType, CapabilityLevel};
use crate::middle::types::lifetime::{Lifetime, LifetimeContext};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OwnershipState {
    /// Variable is owned and can be used or moved
    Owned,
    /// Variable has been moved and cannot be used
    Moved,
    /// Variable is immutably borrowed (shared reference)
    ImmutablyBorrowed,
    /// Variable is mutably borrowed (exclusive reference)
    MutablyBorrowed,
}

#[derive(Debug, Clone)]
pub struct IdentityAwareVariableInfo {
    /// Ownership state of the variable
    pub state: OwnershipState,
    /// Type of the variable
    pub ty: Type,
    /// Identity type information (if applicable)
    pub identity: Option<IdentityType>,
    /// Track how many immutable borrows exist
    pub immutable_borrow_count: usize,
    /// Track if a mutable borrow exists
    pub mutable_borrow_exists: bool,
    /// Lifetime of the variable (for references)
    pub lifetime: Option<Lifetime>,
    /// Required capabilities for using this variable
    pub required_capabilities: Vec<CapabilityLevel>,
}

impl IdentityAwareVariableInfo {
    pub fn new(ty: Type) -> Self {
        Self {
            state: OwnershipState::Owned,
            ty,
            identity: None,
            immutable_borrow_count: 0,
            mutable_borrow_exists: false,
            lifetime: None,
            required_capabilities: Vec::new(),
        }
    }
    
    pub fn with_identity(ty: Type, identity: IdentityType) -> Self {
        let required_capabilities = identity.capabilities.clone();
        Self {
            state: OwnershipState::Owned,
            ty,
            identity: Some(identity),
            immutable_borrow_count: 0,
            mutable_borrow_exists: false,
            lifetime: None,
            required_capabilities,
        }
    }
    
    pub fn can_use(&self) -> bool {
        match self.state {
            OwnershipState::Owned => true,
            OwnershipState::Moved => false,
            OwnershipState::ImmutablyBorrowed => true,
            OwnershipState::MutablyBorrowed => false,
        }
    }
    
    pub fn can_move(&self) -> bool {
        self.state == OwnershipState::Owned
    }
    
    pub fn can_borrow_immutably(&self) -> bool {
        !self.mutable_borrow_exists && self.state != OwnershipState::Moved
    }
    
    pub fn can_borrow_mutably(&self) -> bool {
        self.immutable_borrow_count == 0 && 
        !self.mutable_borrow_exists && 
        self.state == OwnershipState::Owned
    }
    
    /// Check if the variable has the required capabilities for an operation
    pub fn has_capabilities(&self, required: &[CapabilityLevel]) -> bool {
        // If no identity is associated, assume all capabilities are available
        if self.identity.is_none() {
            return true;
        }
        
        // Check if all required capabilities are present
        for req in required {
            if !self.required_capabilities.contains(req) {
                return false;
            }
        }
        true
    }
}

/// Identity-aware borrow checker
#[derive(Debug, Clone)]
pub struct IdentityAwareBorrowChecker {
    /// Map from variable name to its information
    variables: HashMap<String, IdentityAwareVariableInfo>,
    /// Current scope depth
    scope_depth: usize,
    /// Stack of scopes for tracking borrows
    scope_stack: Vec<HashSet<String>>,
    /// Lifetime context for reference tracking
    lifetime_context: LifetimeContext,
    /// Errors collected during checking
    errors: Vec<String>,
}

impl IdentityAwareBorrowChecker {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            scope_depth: 0,
            scope_stack: vec![HashSet::new()],
            lifetime_context: LifetimeContext::new(),
            errors: Vec::new(),
        }
    }
    
    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.scope_depth += 1;
        self.scope_stack.push(HashSet::new());
    }
    
    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        if self.scope_depth > 0 {
            // Remove all variables declared in this scope
            let vars_in_scope = self.scope_stack.pop().unwrap();
            for var_name in vars_in_scope {
                self.variables.remove(&var_name);
            }
            self.scope_depth -= 1;
        }
    }
    
    /// Declare a new variable
    pub fn declare_variable(&mut self, name: String, ty: Type, identity: Option<IdentityType>) {
        let var_info = if let Some(id) = identity {
            IdentityAwareVariableInfo::with_identity(ty, id)
        } else {
            IdentityAwareVariableInfo::new(ty)
        };
        
        self.variables.insert(name.clone(), var_info);
        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope.insert(name);
        }
    }
    
    /// Use a variable (read its value)
    pub fn use_variable(&mut self, name: &str, required_capabilities: &[CapabilityLevel]) -> Result<(), String> {
        if let Some(var_info) = self.variables.get_mut(name) {
            if !var_info.can_use() {
                return Err(format!("Cannot use variable '{}': it has been moved or is mutably borrowed", name));
            }
            
            if !var_info.has_capabilities(required_capabilities) {
                return Err(format!("Insufficient capabilities to use variable '{}'. Required: {:?}, Available: {:?}", 
                    name, required_capabilities, var_info.required_capabilities));
            }
            
            Ok(())
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }
    
    /// Move a variable (transfer ownership)
    pub fn move_variable(&mut self, name: &str, required_capabilities: &[CapabilityLevel]) -> Result<(), String> {
        if let Some(var_info) = self.variables.get_mut(name) {
            if !var_info.can_move() {
                return Err(format!("Cannot move variable '{}': it is already borrowed or moved", name));
            }
            
            if !var_info.has_capabilities(required_capabilities) {
                return Err(format!("Insufficient capabilities to move variable '{}'. Required: {:?}, Available: {:?}", 
                    name, required_capabilities, var_info.required_capabilities));
            }
            
            var_info.state = OwnershipState::Moved;
            Ok(())
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }
    
    /// Create an immutable borrow of a variable
    pub fn borrow_immutable(&mut self, name: &str, required_capabilities: &[CapabilityLevel]) -> Result<(), String> {
        if let Some(var_info) = self.variables.get_mut(name) {
            if !var_info.can_borrow_immutably() {
                return Err(format!("Cannot immutably borrow variable '{}': it is mutably borrowed or moved", name));
            }
            
            if !var_info.has_capabilities(required_capabilities) {
                return Err(format!("Insufficient capabilities to immutably borrow variable '{}'. Required: {:?}, Available: {:?}", 
                    name, required_capabilities, var_info.required_capabilities));
            }
            
            var_info.immutable_borrow_count += 1;
            var_info.state = OwnershipState::ImmutablyBorrowed;
            Ok(())
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }
    
    /// Create a mutable borrow of a variable
    pub fn borrow_mutable(&mut self, name: &str, required_capabilities: &[CapabilityLevel]) -> Result<(), String> {
        if let Some(var_info) = self.variables.get_mut(name) {
            if !var_info.can_borrow_mutably() {
                return Err(format!("Cannot mutably borrow variable '{}': it is already borrowed", name));
            }
            
            if !var_info.has_capabilities(required_capabilities) {
                return Err(format!("Insufficient capabilities to mutably borrow variable '{}'. Required: {:?}, Available: {:?}", 
                    name, required_capabilities, var_info.required_capabilities));
            }
            
            var_info.mutable_borrow_exists = true;
            var_info.state = OwnershipState::MutablyBorrowed;
            Ok(())
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }
    
    /// Release an immutable borrow
    pub fn release_immutable_borrow(&mut self, name: &str) -> Result<(), String> {
        if let Some(var_info) = self.variables.get_mut(name) {
            if var_info.immutable_borrow_count == 0 {
                return Err(format!("No immutable borrow to release for variable '{}'", name));
            }
            
            var_info.immutable_borrow_count -= 1;
            if var_info.immutable_borrow_count == 0 && !var_info.mutable_borrow_exists {
                var_info.state = OwnershipState::Owned;
            }
            Ok(())
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }
    
    /// Release a mutable borrow
    pub fn release_mutable_borrow(&mut self, name: &str) -> Result<(), String> {
        if let Some(var_info) = self.variables.get_mut(name) {
            if !var_info.mutable_borrow_exists {
                return Err(format!("No mutable borrow to release for variable '{}'", name));
            }
            
            var_info.mutable_borrow_exists = false;
            if var_info.immutable_borrow_count == 0 {
                var_info.state = OwnershipState::Owned;
            }
            Ok(())
        } else {
            Err(format!("Variable '{}' not found", name))
        }
    }
    
    /// Check if a variable has identity information
    pub fn has_identity(&self, name: &str) -> bool {
        self.variables.get(name)
            .and_then(|info| info.identity.as_ref())
            .is_some()
    }
    
    /// Get the identity type of a variable
    pub fn get_identity(&self, name: &str) -> Option<&IdentityType> {
        self.variables.get(name)
            .and_then(|info| info.identity.as_ref())
    }
    
    /// Get all errors collected during checking
    pub fn get_errors(&self) -> &[String] {
        &self.errors
    }
    
    /// Clear all errors
    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }
    
    /// Add an error message
    pub fn add_error(&mut self, error: String) {
        self.errors.push(error);
    }
}

/// Capability requirements for common operations
pub mod capability_requirements {
    use super::CapabilityLevel;
    
    /// Capabilities required for reading a value
    pub fn read() -> Vec<CapabilityLevel> {
        vec![CapabilityLevel::Read]
    }
    
    /// Capabilities required for writing a value
    pub fn write() -> Vec<CapabilityLevel> {
        vec![CapabilityLevel::Write]
    }
    
    /// Capabilities required for executing/mutating a value
    pub fn execute() -> Vec<CapabilityLevel> {
        vec![CapabilityLevel::Execute]
    }
    
    /// Capabilities required for full ownership operations
    pub fn owned() -> Vec<CapabilityLevel> {
        vec![CapabilityLevel::Owned]
    }
    
    /// Capabilities required for immutable borrowing
    pub fn immutable_borrow() -> Vec<CapabilityLevel> {
        vec![CapabilityLevel::Read]
    }
    
    /// Capabilities required for mutable borrowing
    pub fn mutable_borrow() -> Vec<CapabilityLevel> {
        vec![CapabilityLevel::Write]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::Type;
    
    #[test]
    fn test_identity_aware_variable_declaration() {
        let mut checker = IdentityAwareBorrowChecker::new();
        
        // Declare a variable without identity
        let ty = Type::I64;
        checker.declare_variable("x".to_string(), ty.clone(), None);
        
        // Declare a variable with identity
        let identity = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
        checker.declare_variable("y".to_string(), ty, Some(identity));
        
        assert!(checker.has_identity("y"));
        assert!(!checker.has_identity("x"));
    }
    
    #[test]
    fn test_capability_checking() {
        let mut checker = IdentityAwareBorrowChecker::new();
        
        // Declare a variable with only read capability
        let identity = IdentityType::new(vec![CapabilityLevel::Read]);
        checker.declare_variable("data".to_string(), Type::I64, Some(identity));
        
        // Should be able to read
        let result = checker.use_variable("data", &[CapabilityLevel::Read]);
        assert!(result.is_ok());
        
        // Should NOT be able to write
        let result = checker.use_variable("data", &[CapabilityLevel::Write]);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Insufficient capabilities"));
    }
    
    #[test]
    fn test_identity_aware_borrowing() {
        let mut checker = IdentityAwareBorrowChecker::new();
        
        // Declare a variable with read/write capabilities
        let identity = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
        checker.declare_variable("data".to_string(), Type::I64, Some(identity));
        
        // Should be able to borrow immutably (requires read)
        let result = checker.borrow_immutable("data", &[CapabilityLevel::Read]);
        assert!(result.is_ok());
        
        // Should be able to borrow mutably (requires write)
        checker.release_immutable_borrow("data").unwrap();
        let result = checker.borrow_mutable("data", &[CapabilityLevel::Write]);
        assert!(result.is_ok());
    }
    
    #[test]
    fn test_scope_management() {
        let mut checker = IdentityAwareBorrowChecker::new();
        
        // Declare variable in outer scope
        checker.declare_variable("outer".to_string(), Type::I64, None);
        
        // Enter inner scope
        checker.enter_scope();
        checker.declare_variable("inner".to_string(), Type::I64, None);
        
        // Both variables should be accessible
        assert!(checker.use_variable("outer", &[]).is_ok());
        assert!(checker.use_variable("inner", &[]).is_ok());
        
        // Exit inner scope
        checker.exit_scope();
        
        // Inner variable should be gone
        assert!(checker.use_variable("inner", &[]).is_err());
        assert!(checker.use_variable("outer", &[]).is_ok());
    }
}