//! Tests for identity-based string operations

use zetac::middle::types::identity::{CapabilityLevel, string_ops};

#[test]
fn test_read_only_string() {
    let s = string_ops::read_only_string("hello".to_string());
    
    // Should be able to read
    assert_eq!(s.get(), "hello");
    assert_eq!(s.len(), 5);
    assert!(s.contains("ell"));
    assert!(s.starts_with("he"));
    assert!(s.ends_with("lo"));
    
    // Should have Read capability
    assert!(s.has_capability(CapabilityLevel::Read));
    assert!(!s.has_capability(CapabilityLevel::Write));
    assert!(!s.has_capability(CapabilityLevel::Owned));
}

#[test]
#[should_panic(expected = "String requires Write capability")]
fn test_read_only_string_cannot_write() {
    let mut s = string_ops::read_only_string("hello".to_string());
    s.append(" world"); // Should panic
}

#[test]
fn test_read_write_string() {
    let mut s = string_ops::read_write_string("hello".to_string());
    
    // Should be able to read
    assert_eq!(s.get(), "hello");
    
    // Should be able to write
    s.append(" world");
    assert_eq!(s.get(), "hello world");
    
    // Should be able to transform
    s.to_uppercase();
    assert_eq!(s.get(), "HELLO WORLD");
    
    // Should have Read and Write capabilities
    assert!(s.has_capability(CapabilityLevel::Read));
    assert!(s.has_capability(CapabilityLevel::Write));
    assert!(!s.has_capability(CapabilityLevel::Owned));
}

#[test]
#[should_panic(expected = "String requires Owned capability")]
fn test_read_write_string_cannot_clone() {
    let s = string_ops::read_write_string("hello".to_string());
    s.clone_string(); // Should panic
}

#[test]
fn test_owned_string() {
    let s = string_ops::owned_string("hello".to_string());
    
    // Should have all capabilities
    assert!(s.has_capability(CapabilityLevel::Read));
    assert!(s.has_capability(CapabilityLevel::Write));
    assert!(s.has_capability(CapabilityLevel::Owned));
    
    // Should be able to clone
    let cloned = s.clone_string();
    assert_eq!(cloned.get(), "hello");
}

#[test]
fn test_string_upgrade() {
    let s = string_ops::read_only_string("hello".to_string());
    assert!(!s.has_capability(CapabilityLevel::Write));
    
    let upgraded = s.upgrade(vec![CapabilityLevel::Write]);
    assert!(upgraded.has_capability(CapabilityLevel::Read));
    assert!(upgraded.has_capability(CapabilityLevel::Write));
}

#[test]
fn test_string_downgrade() {
    let s = string_ops::owned_string("hello".to_string());
    assert!(s.has_capability(CapabilityLevel::Owned));
    
    let downgraded = s.downgrade(vec![CapabilityLevel::Owned]);
    assert!(!downgraded.has_capability(CapabilityLevel::Owned));
    assert!(downgraded.has_capability(CapabilityLevel::Read));
    assert!(downgraded.has_capability(CapabilityLevel::Write));
}

#[test]
fn test_capability_inference() {
    // Test that operations require correct capabilities
    let read_caps = string_ops::infer_string_op_capabilities("len");
    assert_eq!(read_caps, vec![CapabilityLevel::Read]);
    
    let write_caps = string_ops::infer_string_op_capabilities("append");
    assert_eq!(write_caps, vec![CapabilityLevel::Write]);
    
    let owned_caps = string_ops::infer_string_op_capabilities("clone_string");
    assert_eq!(owned_caps, vec![CapabilityLevel::Owned]);
    
    let read_write_caps = string_ops::infer_string_op_capabilities("to_uppercase");
    assert_eq!(read_write_caps, vec![CapabilityLevel::Read, CapabilityLevel::Write]);
}

#[test]
fn test_capability_checking() {
    let read_only = string_ops::read_only_string("test".to_string());
    let read_write = string_ops::read_write_string("test".to_string());
    let owned = string_ops::owned_string("test".to_string());
    
    // Read operations should work for all
    assert!(string_ops::check_string_op_capabilities(&read_only, "len").is_ok());
    assert!(string_ops::check_string_op_capabilities(&read_write, "len").is_ok());
    assert!(string_ops::check_string_op_capabilities(&owned, "len").is_ok());
    
    // Write operations should fail for read-only
    assert!(string_ops::check_string_op_capabilities(&read_only, "append").is_err());
    assert!(string_ops::check_string_op_capabilities(&read_write, "append").is_ok());
    assert!(string_ops::check_string_op_capabilities(&owned, "append").is_ok());
    
    // Owned operations should fail for read-only and read-write
    assert!(string_ops::check_string_op_capabilities(&read_only, "clone_string").is_err());
    assert!(string_ops::check_string_op_capabilities(&read_write, "clone_string").is_err());
    assert!(string_ops::check_string_op_capabilities(&owned, "clone_string").is_ok());
}

#[test]
fn test_string_operations() {
    let mut s = string_ops::read_write_string("  Hello World  ".to_string());
    
    // Test trim
    s.trim();
    assert_eq!(s.get(), "Hello World");
    
    // Test to_uppercase
    s.to_uppercase();
    assert_eq!(s.get(), "HELLO WORLD");
    
    // Test to_lowercase
    s.to_lowercase();
    assert_eq!(s.get(), "hello world");
    
    // Test replace
    s.replace("world", "universe");
    assert_eq!(s.get(), "hello universe");
    
    // Test append
    s.append("!");
    assert_eq!(s.get(), "hello universe!");
    
    // Test contains
    assert!(s.contains("universe"));
    assert!(!s.contains("galaxy"));
    
    // Test starts_with and ends_with
    assert!(s.starts_with("hello"));
    assert!(s.ends_with("!"));
}

#[test]
fn test_string_creation_functions() {
    // Test that creation functions work correctly
    let read_only = string_ops::read_only_string("test".to_string());
    assert_eq!(read_only.capabilities(), &[CapabilityLevel::Read]);
    
    let read_write = string_ops::read_write_string("test".to_string());
    assert_eq!(read_write.capabilities(), &[CapabilityLevel::Read, CapabilityLevel::Write]);
    
    let owned = string_ops::owned_string("test".to_string());
    assert_eq!(owned.capabilities(), &[
        CapabilityLevel::Read,
        CapabilityLevel::Write,
        CapabilityLevel::Owned,
    ]);
}