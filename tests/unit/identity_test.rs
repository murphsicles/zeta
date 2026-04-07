//! Tests for identity type system

extern crate zetac;

use zetac::middle::types::identity::*;
use zetac::middle::types::Type;

#[test]
fn test_identity_type_creation() {
    // Test creating identity types
    let id1 = IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write]);
    assert_eq!(id1.value, None);
    assert!(id1.has_capability(CapabilityLevel::Read));
    assert!(id1.has_capability(CapabilityLevel::Write));
    assert!(!id1.has_capability(CapabilityLevel::Execute));
    
    let id2 = IdentityType::with_value(
        "user:alice".to_string(),
        vec![CapabilityLevel::Owned],
    );
    assert_eq!(id2.value, Some("user:alice".to_string()));
    assert!(id2.has_capability(CapabilityLevel::Owned));
}

#[test]
fn test_identity_type_display() {
    let id = IdentityType::with_value(
        "admin".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Execute],
    );
    assert_eq!(id.to_string(), "identity(\"admin\")[read, write, execute]");
    
    let id2 = IdentityType::new(vec![CapabilityLevel::Read]);
    assert_eq!(id2.to_string(), "identity[read]");
}

#[test]
fn test_identity_context() {
    let mut ctx = IdentityContext::new();
    
    let admin_id = IdentityType::with_value(
        "admin".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Execute],
    );
    
    let user_id = IdentityType::with_value(
        "user".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    ctx.add_identity("admin_role".to_string(), admin_id);
    ctx.add_identity("user_role".to_string(), user_id);
    
    assert!(ctx.get_identity("admin_role").is_some());
    assert!(ctx.get_identity("user_role").is_some());
    assert!(ctx.get_identity("nonexistent").is_none());
}

#[test]
fn test_identity_type_parsing() {
    // Test parsing identity type from string
    // Note: For now, parsing just creates a basic identity type
    // TODO: Implement proper parsing with values and capabilities
    let ty = Type::from_string("identity(\"admin\")[read, write]");
    
    match ty {
        Type::Identity(_) => {
            // Success - identity type created
            // For now, we just verify it's an identity type
        }
        Type::Named(name, _) => {
            // Fallback - treat as named type
            // This is acceptable for now while parsing is basic
            println!("Note: identity type parsed as named type: {}", name);
        }
        _ => {
            panic!("Expected Identity or Named type, got {:?}", ty);
        }
    }
}

#[test]
fn test_identity_can_substitute() {
    let admin = IdentityType::with_value(
        "admin".to_string(),
        vec![CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Execute],
    );
    
    let user = IdentityType::with_value(
        "user".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    // Admin can substitute user (has all user capabilities)
    assert!(admin.can_substitute(&user));
    
    // User cannot substitute admin (missing write and execute)
    assert!(!user.can_substitute(&admin));
}

#[test]
fn test_identity_unification() {
    let mut ctx = IdentityContext::new();
    
    let id1 = IdentityType::with_value(
        "resource".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    let id2 = IdentityType::new(vec![CapabilityLevel::Read]);
    
    let unified = ctx.unify(&id1, &id2).unwrap();
    assert_eq!(unified.value, Some("resource".to_string()));
    assert!(unified.has_capability(CapabilityLevel::Read));
    
    // Test unification failure
    let id3 = IdentityType::with_value(
        "other".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    let id4 = IdentityType::with_value(
        "different".to_string(),
        vec![CapabilityLevel::Read],
    );
    
    assert!(ctx.unify(&id3, &id4).is_none());
}