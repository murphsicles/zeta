//! Tests for parametric identity types

use zetac::middle::types::identity::*;

#[test]
fn test_parametric_identity_creation() {
    // Create a parametric identity type with one type parameter
    let param = IdentityTypeParam {
        name: "T".to_string(),
        constraint: Some(IdentityConstraint::Pattern("read".to_string())),
    };
    
    let identity = IdentityType::parametric(
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
        vec![param],
    );
    
    assert!(identity.is_parametric());
    assert_eq!(identity.type_params.len(), 1);
    assert_eq!(identity.type_params[0].name, "T");
    assert_eq!(identity.capabilities.len(), 2);
    assert!(identity.has_capability(CapabilityLevel::Read));
    assert!(identity.has_capability(CapabilityLevel::Write));
}

#[test]
fn test_parametric_identity_display() {
    // Test display for parametric identity without constraints
    let param1 = IdentityTypeParam {
        name: "T".to_string(),
        constraint: None,
    };
    
    let identity1 = IdentityType::parametric(
        vec![CapabilityLevel::Read],
        vec![param1],
    );
    
    assert_eq!(identity1.to_string(), "identity<T>[read]");
    
    // Test display for parametric identity with constraints
    let param2 = IdentityTypeParam {
        name: "T".to_string(),
        constraint: Some(IdentityConstraint::Pattern("read".to_string())),
    };
    
    let identity2 = IdentityType::parametric(
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
        vec![param2],
    );
    
    assert_eq!(identity2.to_string(), "identity<T: matches 'read'>[read, write]");
    
    // Test display for multiple type parameters
    let param3 = IdentityTypeParam {
        name: "T".to_string(),
        constraint: Some(IdentityConstraint::Pattern("user".to_string())),
    };
    
    let param4 = IdentityTypeParam {
        name: "U".to_string(),
        constraint: Some(IdentityConstraint::MaxLength(10)),
    };
    
    let identity3 = IdentityType::parametric(
        vec![CapabilityLevel::Read],
        vec![param3, param4],
    );
    
    assert_eq!(identity3.to_string(), "identity<T: matches 'user', U: length <= 10>[read]");
}

#[test]
fn test_parametric_identity_instantiation() {
    // Create a parametric identity type
    let param = IdentityTypeParam {
        name: "T".to_string(),
        constraint: Some(IdentityConstraint::Pattern("read".to_string())),
    };
    
    let parametric_identity = IdentityType::parametric(
        vec![CapabilityLevel::Read],
        vec![param],
    );
    
    // Create a concrete identity type to instantiate with
    let concrete_identity = IdentityType::with_value(
        "read_user".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    // Instantiate the parametric identity
    let result = parametric_identity.instantiate(vec![concrete_identity]);
    assert!(result.is_ok());
    
    let instantiated = result.unwrap();
    assert!(!instantiated.is_parametric());
    assert_eq!(instantiated.type_params.len(), 0);
    assert_eq!(instantiated.capabilities.len(), 1);
    assert!(instantiated.has_capability(CapabilityLevel::Read));
}

#[test]
fn test_parametric_identity_instantiation_failure() {
    // Create a parametric identity type with constraint
    let param = IdentityTypeParam {
        name: "T".to_string(),
        constraint: Some(IdentityConstraint::Pattern("admin".to_string())),
    };
    
    let parametric_identity = IdentityType::parametric(
        vec![CapabilityLevel::Read],
        vec![param],
    );
    
    // Try to instantiate with an identity that doesn't satisfy the constraint
    let wrong_identity = IdentityType::with_value(
        "user".to_string(), // Doesn't contain "admin"
        vec![CapabilityLevel::Read],
    );
    
    let result = parametric_identity.instantiate(vec![wrong_identity]);
    assert!(result.is_err());
    
    let error = result.unwrap_err();
    assert!(error.contains("does not satisfy constraint"));
}

#[test]
fn test_parametric_identity_unification() {
    // Create two parametric identities with the same type parameters
    let param = IdentityTypeParam {
        name: "T".to_string(),
        constraint: None,
    };
    
    let identity1 = IdentityType::parametric(
        vec![CapabilityLevel::Read],
        vec![param.clone()],
    );
    
    let identity2 = IdentityType::parametric(
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
        vec![param],
    );
    
    let context = IdentityContext::new();
    let unified = context.unify(&identity1, &identity2);
    
    assert!(unified.is_some());
    let unified_type = unified.unwrap();
    assert!(unified_type.is_parametric());
    assert_eq!(unified_type.type_params.len(), 1);
    assert_eq!(unified_type.capabilities.len(), 2);
    assert!(unified_type.has_capability(CapabilityLevel::Read));
    assert!(unified_type.has_capability(CapabilityLevel::Write));
}

#[test]
fn test_parametric_identity_can_substitute() {
    // Create a parametric identity with Read capability
    let param = IdentityTypeParam {
        name: "T".to_string(),
        constraint: None,
    };
    
    let parametric_read = IdentityType::parametric(
        vec![CapabilityLevel::Read],
        vec![param.clone()],
    );
    
    // Create a parametric identity with Read+Write capabilities
    let parametric_rw = IdentityType::parametric(
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
        vec![param],
    );
    
    // RW can substitute Read (has at least the same capabilities)
    assert!(parametric_rw.can_substitute(&parametric_read));
    
    // Read cannot substitute RW (missing Write capability)
    assert!(!parametric_read.can_substitute(&parametric_rw));
}

#[test]
fn test_parametric_identity_with_multiple_params() {
    // Create a parametric identity with two type parameters
    let param_t = IdentityTypeParam {
        name: "T".to_string(),
        constraint: Some(IdentityConstraint::Pattern("user".to_string())),
    };
    
    let param_u = IdentityTypeParam {
        name: "U".to_string(),
        constraint: Some(IdentityConstraint::MaxLength(20)),
    };
    
    let identity = IdentityType::parametric(
        vec![CapabilityLevel::Read, CapabilityLevel::Write],
        vec![param_t, param_u],
    );
    
    assert!(identity.is_parametric());
    assert_eq!(identity.type_params.len(), 2);
    assert_eq!(identity.type_params[0].name, "T");
    assert_eq!(identity.type_params[1].name, "U");
    assert_eq!(identity.capabilities.len(), 2);
    
    // Test instantiation with two concrete identities
    let concrete_t = IdentityType::with_value(
        "user_alice".to_string(), // Contains "user"
        vec![CapabilityLevel::Read],
    );
    
    let concrete_u = IdentityType::with_value(
        "short".to_string(), // Length <= 20
        vec![CapabilityLevel::Write],
    );
    
    let result = identity.instantiate(vec![concrete_t, concrete_u]);
    assert!(result.is_ok());
    
    let instantiated = result.unwrap();
    assert!(!instantiated.is_parametric());
    assert_eq!(instantiated.type_params.len(), 0);
}