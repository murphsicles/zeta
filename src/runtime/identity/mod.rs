//! Runtime support for identity operations.
//!
//! This module provides runtime checking and validation for identity
//! operations that cannot be fully verified at compile time.

mod validation;
mod bridge;
pub mod integration;

pub use validation::{
    CapabilityValidator, IdentityContext, IdentityValidationHook,
    DefaultValidationHook, ValidationError,
};
pub use bridge::{
    IdentityRuntimeManager, GlobalIdentityRuntime,
    RuntimeIdentityHooks, DefaultRuntimeHooks,
};
pub use integration::{
    init_global_identity_context,
    reset_global_identity_context,
    identity_host_str_concat,
    identity_host_str_len,
    identity_host_str_to_lowercase,
    identity_host_str_to_uppercase,
    identity_host_str_trim,
    identity_host_str_starts_with,
    identity_host_str_ends_with,
    identity_host_str_contains,
    identity_host_str_replace,
};

/// Runtime identity operations module.
pub mod ops {
    use super::*;
    use crate::middle::types::identity::{CapabilityLevel, IdentityType};
    
    /// Create a runtime identity context from a string.
    pub fn create_identity_context(identity_str: &str, capability: CapabilityLevel) -> IdentityContext {
        let identity = IdentityType::with_value(
            identity_str.to_string(),
            vec![capability],
        );
        IdentityContext::new(identity)
    }
    
    /// Validate a string operation at runtime.
    pub fn validate_string_operation(
        context: &IdentityContext,
        operation: &str,
        input: &str,
        args: &[&str],
    ) -> Result<(), ValidationError> {
        // Check capability based on operation type
        let required_capability = match operation {
            "concat" | "substring" | "split" | "find" | "chars" | "as_bytes" | "repeat" 
            | "lines" | "matches" | "rmatches" | "escape_debug" | "escape_default" => CapabilityLevel::Read,
            "trim_start" | "trim_end" | "trim_matches" => CapabilityLevel::Write,
            _ => CapabilityLevel::Read, // Default to read for unknown operations
        };
        
        context.validator().check_capability(required_capability)?;
        
        // Note: IdentityType doesn't have a constraints() method in the current implementation
        // If constraints are needed, they would need to be added to IdentityType
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::identity::CapabilityLevel;
    
    #[test]
    fn test_create_identity_context() {
        let context = ops::create_identity_context("user123", CapabilityLevel::Write);
        assert_eq!(context.validator().capability_level(), CapabilityLevel::Write);
    }
    
    #[test]
    fn test_validate_string_operation() {
        let context = ops::create_identity_context("test", CapabilityLevel::Write);
        
        // Read operation should fail (Write doesn't include Read in current implementation)
        assert!(ops::validate_string_operation(&context, "concat", "hello", &["world"]).is_err());
        
        // Write operation should succeed
        assert!(ops::validate_string_operation(&context, "trim_start", "  hello", &[]).is_ok());
        
        // Test with Read capability
        let read_context = ops::create_identity_context("test2", CapabilityLevel::Read);
        assert!(ops::validate_string_operation(&read_context, "concat", "hello", &["world"]).is_ok());
        assert!(ops::validate_string_operation(&read_context, "trim_start", "  hello", &[]).is_err());
    }
    
    #[test]
    fn test_validate_string_operation_insufficient_capability() {
        let context = ops::create_identity_context("test", CapabilityLevel::Read);
        
        // Read operation should succeed
        assert!(ops::validate_string_operation(&context, "concat", "hello", &["world"]).is_ok());
        
        // Write operation should fail with read-only identity
        assert!(ops::validate_string_operation(&context, "trim_start", "  hello", &[]).is_err());
    }
}