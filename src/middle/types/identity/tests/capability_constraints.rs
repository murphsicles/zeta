//! Tests for identity type capability constraints

use crate::middle::types::identity::*;
use crate::middle::types::identity::inference::*;

#[test]
fn test_capability_constraint_parsing() {
    // Test parsing capability constraints
    let constraint = IdentityConstraint::from_str("read").unwrap();
    assert_eq!(constraint, IdentityConstraint::Capability(CapabilityLevel::Read));
    
    let constraint = IdentityConstraint::from_str("write").unwrap();
    assert_eq!(constraint, IdentityConstraint::Capability(CapabilityLevel::Write));
    
    let constraint = IdentityConstraint::from_str("execute").unwrap();
    assert_eq!(constraint, IdentityConstraint::Capability(CapabilityLevel::Execute));
    
    let constraint = IdentityConstraint::from_str("owned").unwrap();
    assert_eq!(constraint, IdentityConstraint::Capability(CapabilityLevel::Owned));
    
    let constraint = IdentityConstraint::from_str("immutable").unwrap();
    assert_eq!(constraint, IdentityConstraint::Capability(CapabilityLevel::Immutable));
}

#[test]
fn test_capability_constraint_satisfaction() {
    // Create identity with read capability
    let identity = IdentityType::new(vec![CapabilityLevel::Read]);
    
    // Check that it satisfies read constraint
    let constraint = IdentityConstraint::Capability(CapabilityLevel::Read);
    assert!(identity.satisfies_constraint(&constraint));
    
    // Check that it doesn't satisfy write constraint
    let constraint = IdentityConstraint::Capability(CapabilityLevel::Write);
    assert!(!identity.satisfies_constraint(&constraint));
    
    // Create identity with write capability
    let identity = IdentityType::new(vec![CapabilityLevel::Write]);
    
    // Check that it satisfies write constraint
    let constraint = IdentityConstraint::Capability(CapabilityLevel::Write);
    assert!(identity.satisfies_constraint(&constraint));
    
    // Check that it doesn't satisfy owned constraint
    let constraint = IdentityConstraint::Capability(CapabilityLevel::Owned);
    assert!(!identity.satisfies_constraint(&constraint));
}

#[test]
fn test_capability_constraint_inference() {
    let mut ctx = IdentityInferenceContext::new();
    
    // Add type variable with read capability
    ctx.add_type_var(
        "x".to_string(),
        IdentityType::new(vec![CapabilityLevel::Read])
    );
    
    // Add capability constraint requiring read
    ctx.add_constraint(IdentityConstraint::Capability(CapabilityLevel::Read));
    
    // Infer should succeed
    let result = ctx.infer();
    assert!(result.is_ok());
    
    // Now test with insufficient capabilities
    let mut ctx = IdentityInferenceContext::new();
    
    ctx.add_type_var(
        "x".to_string(),
        IdentityType::new(vec![CapabilityLevel::Read])
    );
    
    // Add capability constraint requiring write
    ctx.add_constraint(IdentityConstraint::Capability(CapabilityLevel::Write));
    
    // Infer should fail
    let result = ctx.infer();
    assert!(result.is_err());
    
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());
    assert!(errors[0].contains("does not have required capability"));
}

#[test]
fn test_capability_constraint_with_parametric_types() {
    // Create a parametric identity type with capability constraint
    let type_param = IdentityTypeParam {
        name: "T".to_string(),
        constraint: Some(IdentityConstraint::Capability(CapabilityLevel::Read)),
    };
    
    let parametric_type = IdentityType::parametric(
        vec![CapabilityLevel::Read],
        vec![type_param]
    );
    
    // Instantiate with identity that has read capability
    let concrete_type = IdentityType::new(vec![CapabilityLevel::Read]);
    let result = parametric_type.instantiate(vec![concrete_type]);
    assert!(result.is_ok());
    
    // Instantiate with identity that doesn't have read capability
    let concrete_type = IdentityType::new(vec![CapabilityLevel::Immutable]);
    let result = parametric_type.instantiate(vec![concrete_type]);
    assert!(result.is_err());
    
    let error = result.unwrap_err();
    assert!(error.contains("does not satisfy constraint"));
}

#[test]
fn test_capability_constraint_subtyping() {
    // Create identity with read+write capabilities
    let identity_rw = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
    
    // Create identity with only read capability
    let identity_r = IdentityType::new(vec![CapabilityLevel::Read]);
    
    // Check that rw can substitute r (has at least the same capabilities)
    assert!(identity_rw.can_substitute(&identity_r));
    
    // Check that r cannot substitute rw (missing write capability)
    assert!(!identity_r.can_substitute(&identity_rw));
}

#[test]
fn test_capability_constraint_operations() {
    let ctx = IdentityContext::new();
    let inferencer = CapabilityInferencer::new();
    
    // Test operation that requires read capability
    let identity = IdentityType::new(vec![CapabilityLevel::Read]);
    let result = inferencer.check_capabilities("read_file", &identity);
    assert!(result.is_ok());
    
    // Test operation that requires write capability with read-only identity
    let result = inferencer.check_capabilities("write_file", &identity);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("requires 'write' capability"));
    
    // Test operation that requires write capability with write-enabled identity
    let identity = IdentityType::new(vec![CapabilityLevel::Write]);
    let result = inferencer.check_capabilities("write_file", &identity);
    assert!(result.is_ok());
}

#[test]
fn test_capability_constraint_unification() {
    let ctx = IdentityContext::new();
    
    // Create two identity types with different capabilities
    let t1 = IdentityType::new(vec![CapabilityLevel::Read]);
    let t2 = IdentityType::new(vec![CapabilityLevel::Write]);
    
    // Unify them - should get union of capabilities
    let unified = ctx.unify(&t1, &t2);
    assert!(unified.is_some());
    
    let unified_type = unified.unwrap();
    assert!(unified_type.has_capability(CapabilityLevel::Read));
    assert!(unified_type.has_capability(CapabilityLevel::Write));
    
    // Test with capability constraints
    let mut t1 = IdentityType::new(vec![CapabilityLevel::Read]);
    t1.constraints.push(IdentityConstraint::Capability(CapabilityLevel::Read));
    
    let mut t2 = IdentityType::new(vec![CapabilityLevel::Write]);
    t2.constraints.push(IdentityConstraint::Capability(CapabilityLevel::Write));
    
    // Unify should combine constraints
    let unified = ctx.unify(&t1, &t2);
    assert!(unified.is_some());
    
    let unified_type = unified.unwrap();
    assert_eq!(unified_type.constraints.len(), 2);
}

#[test]
fn test_capability_constraint_verification() {
    // Test the verification pass with capability constraints
    use crate::middle::passes::identity_verification::*;
    
    let mut pass = IdentityVerificationPass::new();
    
    // Create a simple AST that uses an identity-like string
    let ast = AstNode::Program(vec![
        AstNode::Let {
            mut_: false,
            pattern: Box::new(AstNode::Var("auth_token".to_string())),
            ty: Some("String".to_string()),
            expr: Box::new(AstNode::StringLit("secret_auth_token_123".to_string())),
        },
        AstNode::Let {
            mut_: false,
            pattern: Box::new(AstNode::Var("user_id".to_string())),
            ty: Some("String".to_string()),
            expr: Box::new(AstNode::StringLit("user_456".to_string())),
        }
    ]);
    
    let result = pass.verify(&ast);
    assert!(result.is_ok());
    
    // Should have warnings about identity-like strings
    assert!(!pass.warnings().is_empty());
    
    // Check that warnings mention identity types
    let has_identity_warning = pass.warnings().iter()
        .any(|w| w.contains("appears to be an identity"));
    assert!(has_identity_warning);
}