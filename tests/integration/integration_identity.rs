//! Integration tests for identity-aware runtime

#[cfg(test)]
mod tests {
    use zetac::runtime::identity::integration::{
        init_global_identity_context,
        reset_global_identity_context,
        identity_host_str_concat,
        identity_host_str_len,
        identity_host_str_to_lowercase,
        identity_host_str_trim,
        identity_host_str_contains,
        identity_host_str_replace,
    };
    use zetac::runtime::std;
    use core::ptr;
    
    /// Setup function that runs before each test
    fn setup() {
        unsafe {
            reset_global_identity_context();
        }
    }
    
    /// Helper to create a C string and get its pointer
    fn create_c_string(s: &str) -> i64 {
        let bytes = s.as_bytes();
        let len = bytes.len() + 1; // +1 for null terminator
        let ptr = unsafe { std::std_malloc(len) };
        if ptr != 0 {
            unsafe {
                // Copy string bytes
                ptr::copy_nonoverlapping(
                    bytes.as_ptr(),
                    ptr as *mut u8,
                    bytes.len()
                );
                // Add null terminator
                *((ptr as *mut u8).add(bytes.len())) = 0;
            }
        }
        ptr
    }
    
    /// Helper to free a C string
    unsafe fn free_c_string(ptr: i64) {
        if ptr != 0 {
            unsafe {
                std::std_free(ptr as usize);
            }
        }
    }
    
    #[test]
    fn test_read_only_identity() {
        setup();
        
        // Initialize with read-only identity
        let identity_str = create_c_string("read-only-user");
        
        unsafe {
            // Initialize with Read capability (1)
            init_global_identity_context(identity_str, 1);
            
            // Create test strings
            let hello = create_c_string("Hello");
            let world = create_c_string("World");
            
            // Read operations should work
            let len = identity_host_str_len(hello);
            assert!(len > 0, "str_len should work with Read capability");
            
            let contains = identity_host_str_contains(hello, create_c_string("ell"));
            assert_eq!(contains, 1, "str_contains should work with Read capability");
            
            // Write operations should fail (return 0)
            let lower = identity_host_str_to_lowercase(hello);
            assert_eq!(lower, 0, "str_to_lowercase should fail with only Read capability");
            
            let trimmed = identity_host_str_trim(create_c_string("  test  "));
            assert_eq!(trimmed, 0, "str_trim should fail with only Read capability");
            
            let replaced = identity_host_str_replace(hello, create_c_string("ell"), create_c_string("ipp"));
            assert_eq!(replaced, 0, "str_replace should fail with only Read capability");
            
            // Concat requires Read (should work)
            let concat = identity_host_str_concat(hello, world);
            assert!(concat > 0, "str_concat should work with Read capability");
            
            // Clean up
            free_c_string(concat);
            free_c_string(hello);
            free_c_string(world);
            free_c_string(identity_str);
        }
    }
    
    #[test]
    fn test_write_capability() {
        setup();
        
        // Initialize with write capability
        let identity_str = create_c_string("write-user");
        
        unsafe {
            // Initialize with Write capability (2)
            // Note: In a real implementation, we might want to pass multiple capabilities
            // For now, we'll just test that Write operations work
            init_global_identity_context(identity_str, 2);
            
            // Create test string
            let hello = create_c_string("HELLO");
            
            // Write operations should work
            let lower = identity_host_str_to_lowercase(hello);
            assert!(lower > 0, "str_to_lowercase should work with Write capability");
            
            let trimmed = identity_host_str_trim(create_c_string("  test  "));
            assert!(trimmed > 0, "str_trim should work with Write capability");
            
            let replaced = identity_host_str_replace(hello, create_c_string("ELL"), create_c_string("ipp"));
            assert!(replaced > 0, "str_replace should work with Write capability");
            
            // Read operations should fail (Write doesn't include Read)
            let len = identity_host_str_len(hello);
            assert_eq!(len, 0, "str_len should fail with only Write capability");
            
            // Clean up
            free_c_string(lower);
            free_c_string(trimmed);
            free_c_string(replaced);
            free_c_string(hello);
            free_c_string(identity_str);
        }
    }
    
    #[test]
    fn test_no_identity_context_fallback() {
        setup();
        
        // Don't initialize identity context
        
        unsafe {
            // Create test string
            let hello = create_c_string("Hello");
            
            // Without context, should fall back to regular functions
            let len = identity_host_str_len(hello);
            assert!(len > 0, "Should fall back to regular function when no identity context");
            
            // Write operations should also work (no validation)
            let lower = identity_host_str_to_lowercase(hello);
            assert!(lower > 0, "Should fall back to regular function when no identity context");
            
            // Clean up
            free_c_string(lower);
            free_c_string(hello);
        }
    }
    
    #[test]
    fn test_immutable_capability() {
        setup();
        
        // Initialize with immutable capability (lowest level)
        let identity_str = create_c_string("immutable-user");
        
        unsafe {
            // Initialize with Immutable capability (0)
            init_global_identity_context(identity_str, 0);
            
            // Create test string
            let hello = create_c_string("Hello");
            
            // Even read operations should fail with Immutable
            let len = identity_host_str_len(hello);
            assert_eq!(len, 0, "str_len should fail with Immutable capability");
            
            // Write operations should also fail
            let lower = identity_host_str_to_lowercase(hello);
            assert_eq!(lower, 0, "str_to_lowercase should fail with Immutable capability");
            
            // Clean up
            free_c_string(hello);
            free_c_string(identity_str);
        }
    }
}