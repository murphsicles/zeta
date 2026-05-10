// src/frontend/identity_ownership/tests.rs
//! Tests for identity-aware ownership system

use super::*;
use crate::middle::types::Type;

#[test]
fn test_basic_identity_integration() {
    let mut checker = IdentityAwareBorrowChecker::new();
    
    // Test variable declaration with identity
    let identity = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
    checker.declare_variable("my_data".to_string(), Type::I64, Some(identity));
    
    // Verify identity is stored
    assert!(checker.has_identity("my_data"));
    let stored_identity = checker.get_identity("my_data").unwrap();
    assert_eq!(stored_identity.capabilities, vec![CapabilityLevel::Read, CapabilityLevel::Write]);
}

#[test]
fn test_capability_based_access_control() {
    let mut checker = IdentityAwareBorrowChecker::new();
    
    // Create variable with only read capability
    let read_only_identity = IdentityType::new(vec![CapabilityLevel::Read]);
    checker.declare_variable("read_only".to_string(), Type::I64, Some(read_only_identity));
    
    // Create variable with read and write capabilities
    let read_write_identity = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
    checker.declare_variable("read_write".to_string(), Type::I64, Some(read_write_identity));
    
    // Test read operations
    assert!(checker.use_variable("read_only", &[CapabilityLevel::Read]).is_ok());
    assert!(checker.use_variable("read_write", &[CapabilityLevel::Read]).is_ok());
    
    // Test write operations
    assert!(checker.use_variable("read_only", &[CapabilityLevel::Write]).is_err());
    assert!(checker.use_variable("read_write", &[CapabilityLevel::Write]).is_ok());
}

#[test]
fn test_identity_aware_borrowing_rules() {
    let mut checker = IdentityAwareBorrowChecker::new();
    
    // Create variable with capabilities
    let identity = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
    checker.declare_variable("data".to_string(), Type::I64, Some(identity));
    
    // Test immutable borrowing with read capability
    assert!(checker.borrow_immutable("data", &[CapabilityLevel::Read]).is_ok());
    
    // Should not be able to borrow mutably while immutable borrow exists
    assert!(checker.borrow_mutable("data", &[CapabilityLevel::Write]).is_err());
    
    // Release immutable borrow
    assert!(checker.release_immutable_borrow("data").is_ok());
    
    // Now should be able to borrow mutably
    assert!(checker.borrow_mutable("data", &[CapabilityLevel::Write]).is_ok());
    
    // Should not be able to borrow immutably while mutable borrow exists
    assert!(checker.borrow_immutable("data", &[CapabilityLevel::Read]).is_err());
}

#[test]
fn test_move_with_capabilities() {
    let mut checker = IdentityAwareBorrowChecker::new();
    
    // Create variable with ownership capability
    let identity = IdentityType::new(vec![CapabilityLevel::Owned]);
    checker.declare_variable("owned_data".to_string(), Type::I64, Some(identity));
    
    // Should be able to move with ownership capability
    assert!(checker.move_variable("owned_data", &[CapabilityLevel::Owned]).is_ok());
    
    // Variable should now be in moved state
    let var_info = checker.variables.get("owned_data").unwrap();
    assert_eq!(var_info.state, OwnershipState::Moved);
    
    // Should not be able to use moved variable
    assert!(checker.use_variable("owned_data", &[]).is_err());
}

#[test]
fn test_insufficient_capabilities_error() {
    let mut checker = IdentityAwareBorrowChecker::new();
    
    // Create variable with only read capability
    let identity = IdentityType::new(vec![CapabilityLevel::Read]);
    checker.declare_variable("data".to_string(), Type::I64, Some(identity));
    
    // Try to perform write operation (requires write capability)
    let result = checker.use_variable("data", &[CapabilityLevel::Write]);
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.contains("Insufficient capabilities"));
    assert!(error.contains("Required: [Write]"));
}

#[test]
fn test_variable_without_identity() {
    let mut checker = IdentityAwareBorrowChecker::new();
    
    // Declare variable without identity
    checker.declare_variable("regular_var".to_string(), Type::I64, None);
    
    // Should be able to perform any operation (no capability checking)
    assert!(checker.use_variable("regular_var", &[CapabilityLevel::Write]).is_ok());
    assert!(checker.borrow_mutable("regular_var", &[CapabilityLevel::Owned]).is_ok());
    assert!(checker.move_variable("regular_var", &[CapabilityLevel::Execute]).is_ok());
}

#[test]
fn test_multiple_capability_requirements() {
    let mut checker = IdentityAwareBorrowChecker::new();
    
    // Create variable with multiple capabilities
    let identity = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Execute]);
    checker.declare_variable("multi_cap".to_string(), Type::I64, Some(identity));
    
    // Test with single required capability
    assert!(checker.use_variable("multi_cap", &[CapabilityLevel::Read]).is_ok());
    assert!(checker.use_variable("multi_cap", &[CapabilityLevel::Write]).is_ok());
    assert!(checker.use_variable("multi_cap", &[CapabilityLevel::Execute]).is_ok());
    
    // Test with multiple required capabilities
    assert!(checker.use_variable("multi_cap", &[CapabilityLevel::Read, CapabilityLevel::Write]).is_ok());
    assert!(checker.use_variable("multi_cap", &[CapabilityLevel::Read, CapabilityLevel::Execute]).is_ok());
    assert!(checker.use_variable("multi_cap", &[CapabilityLevel::Write, CapabilityLevel::Execute]).is_ok());
    
    // Test with all required capabilities
    assert!(checker.use_variable("multi_cap", &[CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Execute]).is_ok());
}

#[test]
fn test_capability_requirements_module() {
    use super::capability_requirements as req;
    
    // Test capability requirement functions
    assert_eq!(req::read(), vec![CapabilityLevel::Read]);
    assert_eq!(req::write(), vec![CapabilityLevel::Write]);
    assert_eq!(req::execute(), vec![CapabilityLevel::Execute]);
    assert_eq!(req::owned(), vec![CapabilityLevel::Owned]);
    assert_eq!(req::immutable_borrow(), vec![CapabilityLevel::Read]);
    assert_eq!(req::mutable_borrow(), vec![CapabilityLevel::Write]);
}

#[test]
fn test_error_collection() {
    let mut checker = IdentityAwareBorrowChecker::new();
    
    // Declare a variable
    checker.declare_variable("x".to_string(), Type::I64, None);
    
    // Try to use non-existent variable (should add error)
    let _ = checker.use_variable("y", &[]);
    
    // Add custom error
    checker.add_error("Custom error message".to_string());
    
    // Check errors
    let errors = checker.get_errors();
    assert!(errors.len() >= 1);
    assert!(errors.iter().any(|e| e.contains("not found") || e.contains("Custom error")));
    
    // Clear errors
    checker.clear_errors();
    assert!(checker.get_errors().is_empty());
}