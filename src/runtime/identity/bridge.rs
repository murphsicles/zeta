//! Bridge between compile-time identity system and runtime validation.
//!
//! This module provides integration between the compile-time identity
//! type system and the runtime capability validation system.

use crate::middle::types::identity::{IdentityType, CapabilityLevel};
use super::validation::{IdentityContext, ValidationError};

/// Runtime identity manager that tracks identity state across operations.
#[derive(Debug, Clone)]
pub struct IdentityRuntimeManager {
    /// Current identity context.
    context: IdentityContext,
    /// Whether runtime validation is enabled.
    validation_enabled: bool,
}

impl IdentityRuntimeManager {
    /// Create a new identity runtime manager.
    pub fn new(initial_identity: IdentityType) -> Self {
        Self {
            context: IdentityContext::new(initial_identity),
            validation_enabled: true,
        }
    }
    
    /// Disable runtime validation (for performance-critical sections).
    pub fn disable_validation(&mut self) {
        self.validation_enabled = false;
    }
    
    /// Enable runtime validation.
    pub fn enable_validation(&mut self) {
        self.validation_enabled = true;
    }
    
    /// Validate a string operation at runtime.
    pub fn validate_string_operation(
        &self,
        operation: &str,
        input: &str,
        args: &[&str],
    ) -> Result<(), ValidationError> {
        if !self.validation_enabled {
            return Ok(());
        }
        
        // Map operation name to required capability
        let required_capability = match operation {
            // Read operations
            "concat" | "substring" | "split" | "find" | "chars" | "as_bytes" | "repeat" 
            | "lines" | "matches" | "rmatches" | "escape_debug" | "escape_default" 
            | "len" | "is_empty" | "contains" | "starts_with" | "ends_with" => CapabilityLevel::Read,
            
            // Write operations
            "trim_start" | "trim_end" | "trim_matches" | "replace" | "to_lowercase" 
            | "to_uppercase" | "trim" => CapabilityLevel::Write,
            
            // Owned operations
            "into_bytes" | "into_string" => CapabilityLevel::Owned,
            
            // Default to read
            _ => CapabilityLevel::Read,
        };
        
        // Check capability
        self.context.validator().check_capability(required_capability)?;
        
        Ok(())
    }
    
    /// Execute a string operation with runtime validation.
    pub fn execute_string_operation<F, T>(
        &self,
        operation: &str,
        input: &str,
        args: &[&str],
        func: F,
    ) -> Result<T, ValidationError>
    where
        F: FnOnce(&str, &[&str]) -> T,
    {
        self.validate_string_operation(operation, input, args)?;
        Ok(func(input, args))
    }
    
    /// Get the current identity context.
    pub fn context(&self) -> &IdentityContext {
        &self.context
    }
    
    /// Get a mutable reference to the current identity context.
    pub fn context_mut(&mut self) -> &mut IdentityContext {
        &mut self.context
    }
    
    /// Enter a new identity context.
    pub fn enter_context(&mut self, identity: IdentityType) {
        self.context.enter_context(identity);
    }
    
    /// Exit the current identity context.
    pub fn exit_context(&mut self) -> Result<(), ValidationError> {
        self.context.exit_context()
    }
}

/// Global identity runtime state.
#[derive(Debug)]
pub struct GlobalIdentityRuntime {
    /// Current manager (thread-local in a real implementation).
    manager: Option<IdentityRuntimeManager>,
}

impl GlobalIdentityRuntime {
    /// Create a new global identity runtime.
    pub fn new() -> Self {
        Self {
            manager: None,
        }
    }
    
    /// Initialize with a default identity.
    pub fn initialize(&mut self, identity: IdentityType) {
        self.manager = Some(IdentityRuntimeManager::new(identity));
    }
    
    /// Get the current manager, initializing with default if needed.
    pub fn manager(&mut self) -> &mut IdentityRuntimeManager {
        if self.manager.is_none() {
            // Initialize with a default anonymous identity
            let default_identity = IdentityType::with_value(
                "anonymous".to_string(),
                vec![CapabilityLevel::Read],
            );
            self.manager = Some(IdentityRuntimeManager::new(default_identity));
        }
        self.manager.as_mut().unwrap()
    }
    
    /// Check if runtime is initialized.
    pub fn is_initialized(&self) -> bool {
        self.manager.is_some()
    }
}

/// Runtime identity hooks for integration with the compiler.
pub trait RuntimeIdentityHooks: Send + Sync {
    /// Called when a string operation is about to be executed.
    fn before_string_operation(&self, operation: &str, input: &str, args: &[&str]) -> Result<(), ValidationError>;
    
    /// Called after a string operation completes.
    fn after_string_operation(&self, operation: &str, result: &str) -> Result<(), ValidationError>;
    
    /// Called when identity context changes.
    fn on_context_change(&self, old_identity: &IdentityType, new_identity: &IdentityType);
}

/// Default runtime identity hooks.
#[derive(Debug, Clone)]
pub struct DefaultRuntimeHooks;

impl RuntimeIdentityHooks for DefaultRuntimeHooks {
    fn before_string_operation(&self, _operation: &str, _input: &str, _args: &[&str]) -> Result<(), ValidationError> {
        // Default implementation does nothing
        Ok(())
    }
    
    fn after_string_operation(&self, _operation: &str, _result: &str) -> Result<(), ValidationError> {
        // Default implementation does nothing
        Ok(())
    }
    
    fn on_context_change(&self, _old_identity: &IdentityType, _new_identity: &IdentityType) {
        // Default implementation does nothing
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::identity::{IdentityType, CapabilityLevel};
    
    #[test]
    fn test_identity_runtime_manager() {
        let identity = IdentityType::with_value(
            "test".to_string(),
            vec![CapabilityLevel::Read, CapabilityLevel::Write],
        );
        
        let manager = IdentityRuntimeManager::new(identity);
        
        // Should allow read operation
        assert!(manager.validate_string_operation("concat", "hello", &["world"]).is_ok());
        
        // Should allow write operation
        assert!(manager.validate_string_operation("trim_start", "  hello", &[]).is_ok());
        
        // Should fail for owned operation with read-write identity
        assert!(manager.validate_string_operation("into_bytes", "hello", &[]).is_err());
    }
    
    #[test]
    fn test_execute_string_operation() {
        let identity = IdentityType::with_value(
            "test".to_string(),
            vec![CapabilityLevel::Read],
        );
        
        let manager = IdentityRuntimeManager::new(identity);
        
        let result = manager.execute_string_operation(
            "concat",
            "hello",
            &["world"],
            |input, args| format!("{}{}", input, args[0]),
        );
        
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "helloworld");
    }
    
    #[test]
    fn test_validation_disabled() {
        let identity = IdentityType::with_value(
            "test".to_string(),
            vec![CapabilityLevel::Read],
        );
        
        let mut manager = IdentityRuntimeManager::new(identity);
        
        // Disable validation
        manager.disable_validation();
        
        // Should succeed even with insufficient capability
        assert!(manager.validate_string_operation("trim_start", "  hello", &[]).is_ok());
        
        // Re-enable validation
        manager.enable_validation();
        
        // Should fail again
        assert!(manager.validate_string_operation("trim_start", "  hello", &[]).is_err());
    }
    
    #[test]
    fn test_global_identity_runtime() {
        let mut runtime = GlobalIdentityRuntime::new();
        
        // Should initialize on first access
        let manager = runtime.manager();
        assert_eq!(manager.context().validator().capability_level(), CapabilityLevel::Read);
        
        // Re-initialize with different identity
        let new_identity = IdentityType::with_value(
            "user".to_string(),
            vec![CapabilityLevel::Read, CapabilityLevel::Write],
        );
        runtime.initialize(new_identity);
        
        assert!(runtime.is_initialized());
    }
}