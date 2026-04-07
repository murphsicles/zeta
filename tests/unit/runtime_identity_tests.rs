//! Tests for runtime identity support.

use zetac::runtime::identity::{
    IdentityRuntimeManager, GlobalIdentityRuntime, 
    CapabilityValidator, IdentityContext, ValidationError,
    ops,
};
use zetac::middle::types::identity::{IdentityType, CapabilityLevel, IdentityConstraint};

#[test]
fn test_runtime_capability_validation() {
    // Create an identity with read and write capabilities
    let identity = IdentityType::with_value(
        "test_user".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
    );
    
    let validator = CapabilityValidator::new(identity);
    
    // Should allow read operations
    assert!(validator.check_capability(CapabilityLevel::Read).is_ok());
    
    // Should allow write operations
    assert!(validator.check_capability(CapabilityLevel::Write).is_ok());
    
    // Should fail for owned operations
    assert!(validator.check_capability(CapabilityLevel::Owned).is_err());
}

#[test]
fn test_runtime_constraint_validation() {
    let identity = IdentityType::with_value(
        "test".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    let validator = CapabilityValidator::new(identity);
    
    // Valid input for min length
    assert!(validator.check_constraint(&IdentityConstraint::MinLength(3), "test123").is_ok());
    
    // Invalid min length
    assert!(validator.check_constraint(&IdentityConstraint::MinLength(10), "short").is_err());
    
    // Valid max length
    assert!(validator.check_constraint(&IdentityConstraint::MaxLength(10), "short").is_ok());
    
    // Invalid max length
    assert!(validator.check_constraint(&IdentityConstraint::MaxLength(3), "toolong").is_err());
    
    // Pattern match
    assert!(validator.check_constraint(&IdentityConstraint::Pattern("test".to_string()), "test123").is_ok());
    
    // Pattern mismatch
    assert!(validator.check_constraint(&IdentityConstraint::Pattern("test".to_string()), "hello").is_err());
}

#[test]
fn test_capability_constraints() {
    // Create an identity with read and write capabilities
    let identity = IdentityType::with_value(
        "test_user".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
    );
    
    let validator = CapabilityValidator::new(identity);
    
    // Should allow read capability constraint
    assert!(validator.check_constraint(&IdentityConstraint::Capability(CapabilityLevel::Read), "any_value").is_ok());
    
    // Should allow write capability constraint
    assert!(validator.check_constraint(&IdentityConstraint::Capability(CapabilityLevel::Write), "any_value").is_ok());
    
    // Should fail for owned capability constraint
    assert!(validator.check_constraint(&IdentityConstraint::Capability(CapabilityLevel::Owned), "any_value").is_err());
    
    // Should fail for execute capability constraint
    assert!(validator.check_constraint(&IdentityConstraint::Capability(CapabilityLevel::Execute), "any_value").is_err());
    
    // Create an identity with only read capability
    let read_only_identity = IdentityType::with_value(
        "read_only_user".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    let read_only_validator = CapabilityValidator::new(read_only_identity);
    
    // Should allow read capability constraint
    assert!(read_only_validator.check_constraint(&IdentityConstraint::Capability(CapabilityLevel::Read), "any_value").is_ok());
    
    // Should fail for write capability constraint
    assert!(read_only_validator.check_constraint(&IdentityConstraint::Capability(CapabilityLevel::Write), "any_value").is_err());
}

#[test]
fn test_identity_runtime_manager() {
    let identity = IdentityType::with_value(
        "runtime_test".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
    );
    
    let manager = IdentityRuntimeManager::new(identity);
    
    // Test validation of various operations
    assert!(manager.validate_string_operation("concat", "hello", &["world"]).is_ok());
    assert!(manager.validate_string_operation("substring", "hello world", &["0", "5"]).is_ok());
    assert!(manager.validate_string_operation("trim_start", "  hello", &[]).is_ok());
    
    // Should fail for owned operation
    assert!(manager.validate_string_operation("into_bytes", "hello", &[]).is_err());
}

#[test]
fn test_identity_context_switching() {
    let root_identity = IdentityType::with_value(
        "root".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
    );
    
    let mut manager = IdentityRuntimeManager::new(root_identity);
    
    // Enter restricted context
    let restricted_identity = IdentityType::with_value(
        "restricted".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    manager.enter_context(restricted_identity);
    
    // Should fail for write operations in restricted context
    assert!(manager.validate_string_operation("trim_start", "  hello", &[]).is_err());
    
    // Exit back to root context
    assert!(manager.exit_context().is_ok());
    
    // Should succeed for write operations in root context
    assert!(manager.validate_string_operation("trim_start", "  hello", &[]).is_ok());
}

#[test]
fn test_global_identity_runtime() {
    let mut runtime = GlobalIdentityRuntime::new();
    
    // Should initialize with default identity on first access
    let manager = runtime.manager();
    assert_eq!(manager.context().validator().capability_level(), CapabilityLevel::Read);
    
    // Re-initialize with custom identity
    let custom_identity = IdentityType::with_value(
        "custom".to_string(),
        vec![CapabilityLevel::Owned],
    );
    
    runtime.initialize(custom_identity);
    assert!(runtime.is_initialized());
    
    // Verify the new identity is active
    let manager = runtime.manager();
    assert_eq!(manager.context().validator().capability_level(), CapabilityLevel::Owned);
}

#[test]
fn test_runtime_validation_disabled() {
    let identity = IdentityType::with_value(
        "test".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    let mut manager = IdentityRuntimeManager::new(identity);
    
    // With validation enabled, should fail for write operation
    assert!(manager.validate_string_operation("trim_start", "  hello", &[]).is_err());
    
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
fn test_execute_with_validation() {
    let identity = IdentityType::with_value(
        "executor".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
    );
    
    let manager = IdentityRuntimeManager::new(identity);
    
    // Execute a valid operation
    let result = manager.execute_string_operation(
        "concat",
        "hello",
        &["world"],
        |input, args| format!("{}{}", input, args[0]),
    );
    
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "helloworld");
    
    // Try to execute an invalid operation
    let invalid_result = manager.execute_string_operation(
        "into_bytes",  // Requires Owned capability
        "hello",
        &[],
        |input, _| input.as_bytes().to_vec(),
    );
    
    assert!(invalid_result.is_err());
}

#[test]
fn test_capability_escalation() {
    let identity = IdentityType::with_value(
        "escalation_test".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    let mut validator = CapabilityValidator::new(identity)
        .with_escalation(true)
        .with_audit_logging(true);
    
    // Initially only has read capability
    assert!(validator.check_capability(CapabilityLevel::Read).is_ok());
    assert!(validator.check_capability(CapabilityLevel::Write).is_err());
    
    // Escalate to write
    assert!(validator.escalate_capability(CapabilityLevel::Write).is_ok());
    
    // Now should have write capability
    assert!(validator.check_capability(CapabilityLevel::Write).is_ok());
    
    // De-escalate back to read
    assert!(validator.escalate_capability(CapabilityLevel::Read).is_ok());
    
    // Should be back to read-only
    assert!(validator.check_capability(CapabilityLevel::Write).is_err());
}

#[test]
fn test_validation_error_messages() {
    let identity = IdentityType::with_value(
        "error_test".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    let validator = CapabilityValidator::new(identity);
    
    // Test missing capability error
    let error = validator.check_capability(CapabilityLevel::Write)
        .unwrap_err();
    
    match error {
        ValidationError::MissingCapability { required, actual } => {
            assert_eq!(required, CapabilityLevel::Write);
            assert_eq!(actual, CapabilityLevel::Read);
        }
        _ => panic!("Expected MissingCapability error"),
    }
    
    // Test constraint violation error
    let error = validator.check_constraint(
        &IdentityConstraint::MinLength(5),
        "hi",
    ).unwrap_err();
    
    match error {
        ValidationError::ConstraintViolation { constraint, value } => {
            assert!(matches!(constraint, IdentityConstraint::MinLength(5)));
            assert_eq!(value, "hi");
        }
        _ => panic!("Expected ConstraintViolation error"),
    }
}

#[test]
fn test_ops_module() {
    // Test creating identity context
    let context = ops::create_identity_context("ops_test", CapabilityLevel::Write);
    assert_eq!(context.validator().capability_level(), CapabilityLevel::Write);
    
    // Test validation through ops module
    // Write context should fail for read operations
    assert!(ops::validate_string_operation(&context, "concat", "hello", &["world"]).is_err());
    // Write context should succeed for write operations
    assert!(ops::validate_string_operation(&context, "trim_start", "  hello", &[]).is_ok());
    
    // Test with read-only context
    let read_only_context = ops::create_identity_context("read_only", CapabilityLevel::Read);
    assert!(ops::validate_string_operation(&read_only_context, "concat", "hello", &["world"]).is_ok());
    assert!(ops::validate_string_operation(&read_only_context, "trim_start", "  hello", &[]).is_err());
}