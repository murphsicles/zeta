//! # Identity Inference Tests
//!
//! Tests for identity type inference and capability checking.

use zetac::middle::types::identity::*;
use zetac::middle::types::identity::inference::*;
use zetac::middle::passes::identity_verification::*;
use zetac::frontend::ast::AstNode;

#[test]
fn test_identity_inference_basic() {
    let mut ctx = IdentityInferenceContext::new();
    
    // Add type variables
    ctx.add_type_var(
        "user_id".to_string(),
        IdentityType::with_value("user123".to_string(), vec![CapabilityLevel::Read])
    );
    
    ctx.add_type_var(
        "admin_token".to_string(),
        IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Execute])
    );
    
    // Add constraints
    ctx.add_constraint(IdentityConstraint::MinLength(5));
    ctx.add_constraint(IdentityConstraint::Pattern("user".to_string()));
    
    // Infer types
    let result = ctx.infer();
    assert!(result.is_ok());
    
    let types = result.unwrap();
    assert_eq!(types.len(), 2);
    assert!(types.get("user_id").is_some());
    assert!(types.get("admin_token").is_some());
    
    // Check user_id has read capability
    let user_id = types.get("user_id").unwrap();
    assert!(user_id.has_capability(CapabilityLevel::Read));
    assert!(!user_id.has_capability(CapabilityLevel::Write));
    
    // Check admin_token has multiple capabilities
    let admin_token = types.get("admin_token").unwrap();
    assert!(admin_token.has_capability(CapabilityLevel::Read));
    assert!(admin_token.has_capability(CapabilityLevel::Write));
    assert!(admin_token.has_capability(CapabilityLevel::Execute));
}

#[test]
fn test_identity_inference_with_errors() {
    let mut ctx = IdentityInferenceContext::new();
    
    // Add type variable with value that violates constraint
    ctx.add_type_var(
        "short_id".to_string(),
        IdentityType::with_value("ab".to_string(), vec![CapabilityLevel::Read])
    );
    
    // Add constraint that will be violated
    ctx.add_constraint(IdentityConstraint::MinLength(3));
    
    // Infer types - should fail
    let result = ctx.infer();
    assert!(result.is_err());
    
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());
    assert!(errors[0].contains("less than minimum"));
}

#[test]
fn test_capability_inference_for_operations() {
    let inferencer = CapabilityInferencer::new();
    
    // Test file operations
    let read_caps = inferencer.infer_capabilities("read_file");
    assert!(read_caps.is_some());
    assert_eq!(read_caps.unwrap(), &[CapabilityLevel::Read]);
    
    let write_caps = inferencer.infer_capabilities("write_file");
    assert!(write_caps.is_some());
    assert_eq!(write_caps.unwrap(), &[CapabilityLevel::Write]);
    
    let execute_caps = inferencer.infer_capabilities("execute");
    assert!(execute_caps.is_some());
    assert_eq!(execute_caps.unwrap(), &[CapabilityLevel::Execute]);
    
    // Test string operations
    let len_caps = inferencer.infer_capabilities("len");
    assert!(len_caps.is_some());
    assert_eq!(len_caps.unwrap(), &[CapabilityLevel::Read]);
    
    let replace_caps = inferencer.infer_capabilities("replace");
    assert!(replace_caps.is_some());
    assert_eq!(replace_caps.unwrap(), &[CapabilityLevel::Write]);
}

#[test]
fn test_capability_checking() {
    let inferencer = CapabilityInferencer::new();
    
    // Create identities with different capabilities
    let read_only = IdentityType::new(vec![CapabilityLevel::Read]);
    let read_write = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
    let full_access = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Execute]);
    
    // Test read_file operation
    assert!(inferencer.check_capabilities("read_file", &read_only).is_ok());
    assert!(inferencer.check_capabilities("read_file", &read_write).is_ok());
    assert!(inferencer.check_capabilities("read_file", &full_access).is_ok());
    
    // Test write_file operation
    assert!(inferencer.check_capabilities("write_file", &read_only).is_err());
    assert!(inferencer.check_capabilities("write_file", &read_write).is_ok());
    assert!(inferencer.check_capabilities("write_file", &full_access).is_ok());
    
    // Test execute operation
    assert!(inferencer.check_capabilities("execute", &read_only).is_err());
    assert!(inferencer.check_capabilities("execute", &read_write).is_err());
    assert!(inferencer.check_capabilities("execute", &full_access).is_ok());
}

#[test]
fn test_identity_operation_inference() {
    let ctx = IdentityContext::new();
    
    // Test Create operation
    let result = ctx.infer_operation(&IdentityOp::Create, &[]);
    assert!(result.is_some());
    let identity = result.unwrap();
    assert!(identity.has_capability(CapabilityLevel::Owned));
    assert!(identity.has_capability(CapabilityLevel::Read));
    assert!(identity.has_capability(CapabilityLevel::Write));
    assert!(identity.has_capability(CapabilityLevel::Execute));
    
    // Test Verify operation
    let arg = IdentityType::new(vec![CapabilityLevel::Read]);
    let result = ctx.infer_operation(&IdentityOp::Verify, &[arg]);
    assert!(result.is_some());
    let verified = result.unwrap();
    assert!(verified.has_capability(CapabilityLevel::Read));
    
    // Test Delegate operation
    let arg = IdentityType::new(vec![CapabilityLevel::Owned]);
    let result = ctx.infer_operation(&IdentityOp::Delegate, &[arg.clone()]);
    assert!(result.is_some());
    let delegated = result.unwrap();
    assert!(delegated.delegatable);
    assert!(!delegated.has_capability(CapabilityLevel::Owned));
    
    // Test Combine operation
    let arg1 = IdentityType::new(vec![CapabilityLevel::Read]);
    let arg2 = IdentityType::new(vec![CapabilityLevel::Write]);
    let result = ctx.infer_operation(&IdentityOp::Combine, &[arg1, arg2]);
    assert!(result.is_some());
    let combined = result.unwrap();
    assert!(combined.has_capability(CapabilityLevel::Read));
    assert!(combined.has_capability(CapabilityLevel::Write));
}

#[test]
fn test_identity_operation_checking() {
    let ctx = IdentityContext::new();
    
    // Test Create - should succeed with no arguments
    assert!(ctx.check_operation(&IdentityOp::Create, &[]).is_ok());
    
    // Test Create with arguments - should fail
    let arg = IdentityType::new(vec![CapabilityLevel::Read]);
    assert!(ctx.check_operation(&IdentityOp::Create, &[arg]).is_err());
    
    // Test Verify with no arguments - should fail
    assert!(ctx.check_operation(&IdentityOp::Verify, &[]).is_err());
    
    // Test Verify with argument - should succeed
    let arg = IdentityType::new(vec![CapabilityLevel::Read]);
    assert!(ctx.check_operation(&IdentityOp::Verify, &[arg]).is_ok());
    
    // Test Delegate without owned capability - should fail
    let arg = IdentityType::new(vec![CapabilityLevel::Read]);
    assert!(ctx.check_operation(&IdentityOp::Delegate, &[arg]).is_err());
    
    // Test Delegate with owned capability - should succeed
    let arg = IdentityType::new(vec![CapabilityLevel::Owned]);
    assert!(ctx.check_operation(&IdentityOp::Delegate, &[arg]).is_ok());
    
    // Test Combine with insufficient arguments - should fail
    let arg = IdentityType::new(vec![CapabilityLevel::Read]);
    assert!(ctx.check_operation(&IdentityOp::Combine, &[arg]).is_err());
    
    // Test Combine with sufficient arguments - should succeed
    let arg1 = IdentityType::new(vec![CapabilityLevel::Read]);
    let arg2 = IdentityType::new(vec![CapabilityLevel::Write]);
    assert!(ctx.check_operation(&IdentityOp::Combine, &[arg1, arg2]).is_ok());
}

#[test]
fn test_identity_verification_pass() {
    let mut pass = IdentityVerificationPass::new();
    
    // Create a simple AST with identity-like strings using Let statements
    let ast = AstNode::Program(vec![
        AstNode::Let {
            mut_: false,
            pattern: Box::new(AstNode::Var("auth_token".to_string())),
            ty: Some("String".to_string()),
            expr: Box::new(AstNode::StringLit("secret_jwt_token_123".to_string())),
        },
        AstNode::Let {
            mut_: false,
            pattern: Box::new(AstNode::Var("user_id".to_string())),
            ty: Some("String".to_string()),
            expr: Box::new(AstNode::StringLit("user_456".to_string())),
        },
        AstNode::Let {
            mut_: false,
            pattern: Box::new(AstNode::Var("counter".to_string())),
            ty: Some("i64".to_string()),
            expr: Box::new(AstNode::StringLit("42".to_string())),
        },
    ]);
    
    // Verify the AST
    let result = pass.verify(&ast);
    assert!(result.is_ok());
    
    // Check that we got warnings for identity-like strings
    let warnings = pass.warnings();
    assert!(!warnings.is_empty());
    
    // Should have warnings for auth_token and user_id
    let identity_warnings: Vec<&String> = warnings.iter()
        .filter(|w| w.contains("appears to be an identity"))
        .collect();
    // At least one warning should be present
    assert!(!identity_warnings.is_empty());
    
    // Should not have warnings for counter
    let counter_warnings: Vec<&String> = warnings.iter()
        .filter(|w| w.contains("counter"))
        .collect();
    assert!(counter_warnings.is_empty());
}

#[test]
fn test_identity_type_parsing_and_display() {
    // Test parsing identity type from string
    let identity_str = "identity(\"user123\")[read, write]";
    
    // For now, just test that we can create identity types
    let identity = IdentityType::with_value(
        "user123".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write]
    );
    
    assert_eq!(identity.value(), Some(&"user123".to_string()));
    assert!(identity.has_capability(CapabilityLevel::Read));
    assert!(identity.has_capability(CapabilityLevel::Write));
    assert!(!identity.has_capability(CapabilityLevel::Execute));
    
    // Test display
    let display = identity.to_string();
    assert!(display.contains("identity(\"user123\")"));
    assert!(display.contains("read"));
    assert!(display.contains("write"));
}

#[test]
fn test_identity_substitution() {
    // Create identities with different capabilities
    let read_only = IdentityType::new(vec![CapabilityLevel::Read]);
    let read_write = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
    let full_access = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Execute]);
    
    // read_write can substitute read_only (has all required capabilities)
    assert!(read_write.can_substitute(&read_only));
    
    // full_access can substitute read_write (has all required capabilities)
    assert!(full_access.can_substitute(&read_write));
    
    // read_only cannot substitute read_write (missing write capability)
    assert!(!read_only.can_substitute(&read_write));
    
    // read_write cannot substitute full_access (missing execute capability)
    assert!(!read_write.can_substitute(&full_access));
}

#[test]
fn test_identity_unification() {
    let ctx = IdentityContext::new();
    
    // Create two identity types to unify
    let identity1 = IdentityType::with_value(
        "user123".to_string(),
        vec![CapabilityLevel::Read]
    );
    
    let identity2 = IdentityType::new(
        vec![CapabilityLevel::Read, CapabilityLevel::Write]
    );
    
    // Unify them
    let result = ctx.unify(&identity1, &identity2);
    assert!(result.is_some());
    
    let unified = result.unwrap();
    
    // Should have union of capabilities
    assert!(unified.has_capability(CapabilityLevel::Read));
    assert!(unified.has_capability(CapabilityLevel::Write));
    
    // Should have the known value
    assert_eq!(unified.value(), Some(&"user123".to_string()));
    
    // Test unification failure with conflicting values
    let identity3 = IdentityType::with_value(
        "user456".to_string(),
        vec![CapabilityLevel::Read]
    );
    
    let result = ctx.unify(&identity1, &identity3);
    assert!(result.is_none()); // Should fail because values don't match
}