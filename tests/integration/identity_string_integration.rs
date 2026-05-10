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
    
    #[test]
    fn test_substring_operation() {
        // Test substring operation
        let str1 = read_write_string("Hello World".to_string());
        
        // Get substring
        let substring = str1.substring(0, 5);
        assert_eq!(substring.get(), "Hello");
        
        // Substring should have same capabilities
        assert!(substring.has_capability(CapabilityLevel::Read));
        assert!(substring.has_capability(CapabilityLevel::Write));
        
        // Test with read-only string
        let read_only_str = read_only_string("ReadOnlyString".to_string());
        let read_only_substring = read_only_str.substring(0, 8);
        assert_eq!(read_only_substring.get(), "ReadOnly");
        assert!(read_only_substring.has_capability(CapabilityLevel::Read));
        assert!(!read_only_substring.has_capability(CapabilityLevel::Write));
    }
    
    #[test]
    fn test_concat_operation() {
        // Test concat operation with capability intersection
        let str1 = read_write_string("Hello ".to_string());
        let str2 = read_write_string("World".to_string());
        
        // Both have Read+Write, result should have Read+Write
        let concatenated = str1.concat(&str2);
        assert_eq!(concatenated.get(), "Hello World");
        assert!(concatenated.has_capability(CapabilityLevel::Read));
        assert!(concatenated.has_capability(CapabilityLevel::Write));
        
        // Test with read-only string (intersection should be Read only)
        let read_only_str = read_only_string("ReadOnly ".to_string());
        let read_write_str = read_write_string("ReadWrite".to_string());
        
        let mixed_concat = read_only_str.concat(&read_write_str);
        assert_eq!(mixed_concat.get(), "ReadOnly ReadWrite");
        assert!(mixed_concat.has_capability(CapabilityLevel::Read));
        assert!(!mixed_concat.has_capability(CapabilityLevel::Write)); // Write not in intersection
    }
    
    #[test]
    fn test_split_operation() {
        // Test split operation
        let str1 = read_write_string("Hello,World,Zeta".to_string());
        
        let parts = str1.split(",");
        assert_eq!(parts.len(), 3);
        assert_eq!(parts[0].get(), "Hello");
        assert_eq!(parts[1].get(), "World");
        assert_eq!(parts[2].get(), "Zeta");
        
        // All parts should have same capabilities
        for part in &parts {
            assert!(part.has_capability(CapabilityLevel::Read));
            assert!(part.has_capability(CapabilityLevel::Write));
        }
    }
    
    #[test]
    fn test_find_operation() {
        // Test find operation
        let str1 = read_only_string("Hello World".to_string());
        
        assert_eq!(str1.find("World"), Some(6));
        assert_eq!(str1.find("Zeta"), None);
        assert_eq!(str1.find("o"), Some(4)); // First 'o' in "Hello"
    }
    
    #[test]
    #[should_panic(expected = "String requires Read capability for substring()")]
    fn test_substring_without_read_capability() {
        // This test should panic because we're trying to create a string without Read capability
        // and then call substring on it
        // Note: We can't actually create a StringWithIdentity without Read capability
        // using the public API, so this test is more conceptual
        // For now, we'll test with a string that has no capabilities (not possible with public API)
        // Instead, we'll test the capability checking function
        let no_cap_string = StringWithIdentity::new("test".to_string(), vec![]);
        let _ = no_cap_string.substring(0, 2);
    }
    
    #[test]
    fn test_capability_propagation_rules() {
        // Test capability propagation rules for string operations
        
        // Rule 1: substring preserves all capabilities
        let str1 = owned_string("Hello World".to_string());
        let substring = str1.substring(0, 5);
        assert!(substring.has_capability(CapabilityLevel::Read));
        assert!(substring.has_capability(CapabilityLevel::Write));
        assert!(substring.has_capability(CapabilityLevel::Owned));
        
        // Rule 2: concat results in intersection of capabilities
        let read_only = read_only_string("ReadOnly".to_string());
        let read_write = read_write_string("ReadWrite".to_string());
        let owned = owned_string("Owned".to_string());
        
        // read_only ∩ read_write = read_only
        let concat1 = read_only.concat(&read_write);
        assert!(concat1.has_capability(CapabilityLevel::Read));
        assert!(!concat1.has_capability(CapabilityLevel::Write));
        
        // read_write ∩ owned = read_write (owned is not in read_write)
        let concat2 = read_write.concat(&owned);
        assert!(concat2.has_capability(CapabilityLevel::Read));
        assert!(concat2.has_capability(CapabilityLevel::Write));
        assert!(!concat2.has_capability(CapabilityLevel::Owned));
        
        // Rule 3: split preserves all capabilities
        let csv = read_write_string("a,b,c".to_string());
        let parts = csv.split(",");
        for part in parts {
            assert!(part.has_capability(CapabilityLevel::Read));
            assert!(part.has_capability(CapabilityLevel::Write));
        }
    }
    
    #[test]
    fn test_new_string_operations() {
        // Test the newly added string operations
        
        // Test chars() operation
        let str1 = read_only_string("Hello".to_string());
        let chars: Vec<char> = str1.chars().collect();
        assert_eq!(chars, vec!['H', 'e', 'l', 'l', 'o']);
        
        // Test as_bytes() operation
        let bytes = str1.as_bytes();
        assert_eq!(bytes, b"Hello");
        
        // Test repeat() operation
        let repeated = str1.repeat(3);
        assert_eq!(repeated.get(), "HelloHelloHello");
        assert!(repeated.has_capability(CapabilityLevel::Read));
        
        // Test trim_start() and trim_end()
        let mut str2 = read_write_string("  Hello World  ".to_string());
        str2.trim_start();
        assert_eq!(str2.get(), "Hello World  ");
        
        str2.trim_end();
        assert_eq!(str2.get(), "Hello World");
        
        // Test lines() operation
        let multiline = read_only_string("Line 1\nLine 2\nLine 3".to_string());
        let lines = multiline.lines();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0].get(), "Line 1");
        assert_eq!(lines[1].get(), "Line 2");
        assert_eq!(lines[2].get(), "Line 3");
        
        // Test matches() operation
        let text = read_only_string("apple banana apple cherry".to_string());
        let matches = text.matches("apple");
        assert_eq!(matches, vec!["apple".to_string(), "apple".to_string()]);
        
        // Test rmatches() operation
        let rmatches = text.rmatches("apple");
        assert_eq!(rmatches, vec!["apple".to_string(), "apple".to_string()]);
        
        // Test trim_matches() operation
        let mut str3 = read_write_string("xxxHelloxxx".to_string());
        str3.trim_matches('x'); // Use char pattern instead of &str
        assert_eq!(str3.get(), "Hello");
        
        // Test escape_debug() operation
        let special = read_only_string("Hello\nWorld\t!".to_string());
        let escaped = special.escape_debug();
        assert!(escaped.get().contains("\\n"));
        assert!(escaped.get().contains("\\t"));
        
        // Test escape_default() operation
        let escaped_default = special.escape_default();
        assert!(escaped_default.get().contains("\\n"));
        assert!(escaped_default.get().contains("\\t"));
    }
    
    #[test]
    fn test_capability_checking_for_new_operations() {
        // Test capability checking for the new operations
        
        let read_only_str = read_only_string("test".to_string());
        let read_write_str = read_write_string("test".to_string());
        
        // Operations that only require Read capability
        let read_ops = vec!["chars", "as_bytes", "repeat", "lines", "matches", "rmatches", "escape_debug", "escape_default"];
        for op in read_ops {
            let result = check_string_op_capabilities(&read_only_str, op);
            assert!(result.is_ok(), "Operation '{}' should work with read-only string", op);
        }
        
        // Operations that require Read+Write capability
        let read_write_ops = vec!["trim_start", "trim_end", "trim_matches"];
        for op in read_write_ops {
            let result = check_string_op_capabilities(&read_only_str, op);
            assert!(result.is_err(), "Operation '{}' should fail with read-only string", op);
            
            let result = check_string_op_capabilities(&read_write_str, op);
            assert!(result.is_ok(), "Operation '{}' should work with read-write string", op);
        }
    }
}