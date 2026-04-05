//! Integration tests for identity-aware string operations

#[cfg(test)]
mod tests {
    use zetac::middle::types::identity::string_ops::*;
    use zetac::middle::types::identity::*;

    #[test]
    fn test_string_with_identity_integration() {
        // Test basic string creation with identity using helper functions
        let read_only_str = read_only_string("hello".to_string());
        assert_eq!(read_only_str.get(), "hello");
        assert!(read_only_str.has_capability(CapabilityLevel::Read));
        
        let read_write_str = read_write_string("world".to_string());
        assert_eq!(read_write_str.get(), "world");
        assert!(read_write_str.has_capability(CapabilityLevel::Read));
        assert!(read_write_str.has_capability(CapabilityLevel::Write));
        
        let owned_str = owned_string("owned".to_string());
        assert_eq!(owned_str.get(), "owned");
        assert!(owned_str.has_capability(CapabilityLevel::Read));
        assert!(owned_str.has_capability(CapabilityLevel::Write));
        assert!(owned_str.has_capability(CapabilityLevel::Owned));
    }

    #[test]
    fn test_capability_inference() {
        // Test capability inference for string operations
        let str1 = read_only_string("hello".to_string());
        let str2 = read_write_string("world".to_string());
        
        // Check that read-only string has read capability
        assert!(str1.has_capability(CapabilityLevel::Read));
        assert!(!str1.has_capability(CapabilityLevel::Write));
        
        // Check that read-write string has both capabilities
        assert!(str2.has_capability(CapabilityLevel::Read));
        assert!(str2.has_capability(CapabilityLevel::Write));
    }

    #[test]
    fn test_identity_type_integration() {
        // Test that identity types work with the type system
        let identity_type = IdentityType::with_value("test".to_string(), vec![CapabilityLevel::Read, CapabilityLevel::Write]);
        
        assert_eq!(identity_type.value(), Some(&"test".to_string()));
        assert!(identity_type.has_capability(CapabilityLevel::Read));
        assert!(identity_type.has_capability(CapabilityLevel::Write));
        assert_eq!(identity_type.capabilities().len(), 2);
    }

    #[test]
    fn test_string_operations_with_identity() {
        // Test various string operations with identity
        let mut str1 = read_write_string("Hello".to_string());
        let str2 = read_only_string(" World".to_string());
        
        // Test length (requires Read capability)
        assert_eq!(str1.len(), 5);
        assert_eq!(str2.len(), 6);
        
        // Test contains (requires Read capability)
        assert!(str1.contains("ell"));
        assert!(str2.contains("World"));
        
        // Test starts_with (requires Read capability)
        assert!(str1.starts_with("Hel"));
        assert!(str2.starts_with(" Wor"));
        
        // Test ends_with (requires Read capability)
        assert!(str1.ends_with("lo"));
        assert!(str2.ends_with("rld"));
        
        // Test append (requires Write capability)
        str1.append(" there");
        assert_eq!(str1.get(), "Hello there");
    }

    #[test]
    fn test_capability_upgrade_downgrade() {
        // Test capability level changes
        let str1 = read_write_string("test".to_string());
        
        // Downgrade to read-only
        let str1_read_only = str1.downgrade(vec![CapabilityLevel::Write]);
        assert!(str1_read_only.has_capability(CapabilityLevel::Read));
        assert!(!str1_read_only.has_capability(CapabilityLevel::Write));
        
        // Upgrade back to read-write
        let str1_read_write = str1_read_only.upgrade(vec![CapabilityLevel::Write]);
        assert!(str1_read_write.has_capability(CapabilityLevel::Read));
        assert!(str1_read_write.has_capability(CapabilityLevel::Write));
    }

    #[test]
    fn test_capability_checking() {
        // Test that operations fail without required capabilities
        let read_only_str = read_only_string("test".to_string());
        
        // These should work (read operations)
        assert_eq!(read_only_str.len(), 4);
        assert!(read_only_str.contains("es"));
        
        // Try to create a mutable reference (should panic at runtime in actual usage)
        // We can't test this directly since it would panic
        // Instead, we test the capability checking function
        let result = check_string_op_capabilities(&read_only_str, "append");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("requires Write capability"));
        
        // Read operations should pass
        let result = check_string_op_capabilities(&read_only_str, "len");
        assert!(result.is_ok());
    }

    #[test]
    fn test_string_cloning() {
        // Test string cloning with owned capability
        let owned_str = owned_string("original".to_string());
        
        // Clone should work (requires Owned capability)
        let cloned = owned_str.clone_string();
        assert_eq!(cloned.get(), "original");
        
        // Try to clone a read-only string (should fail)
        let read_only_str = read_only_string("readonly".to_string());
        // This would panic at runtime, so we test the capability check
        let result = check_string_op_capabilities(&read_only_str, "clone_string");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("requires Owned capability"));
    }

    #[test]
    fn test_string_transformations() {
        // Test string transformation operations
        let mut str1 = read_write_string("  Hello World  ".to_string());
        
        // Test trim (requires Read and Write)
        str1.trim();
        assert_eq!(str1.get(), "Hello World");
        
        // Test to_uppercase (requires Read and Write)
        str1.to_uppercase();
        assert_eq!(str1.get(), "HELLO WORLD");
        
        // Test to_lowercase (requires Read and Write)
        str1.to_lowercase();
        assert_eq!(str1.get(), "hello world");
        
        // Test replace (requires Read and Write)
        str1.replace("world", "Zeta");
        assert_eq!(str1.get(), "hello Zeta");
    }
}