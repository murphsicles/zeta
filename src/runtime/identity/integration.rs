//! Integration between identity runtime and compiler runtime.
//!
//! This module provides identity-aware versions of runtime functions
//! that wrap existing host functions with capability validation.

use crate::middle::types::identity::{CapabilityLevel, IdentityType};
use crate::runtime::identity::validation::IdentityContext;
use std::ffi::{CStr, CString, c_char};
use std::ptr;

/// Global identity runtime context.
/// In a real implementation, this would be thread-local or stored in TLS.
static mut GLOBAL_IDENTITY_CONTEXT: Option<IdentityContext> = None;

/// Initialize the global identity context.
/// Must be called before any identity-aware operations.
///
/// # Safety
/// This function is not thread-safe.
/// identity_ptr: pointer to identity string (null-terminated C string)
/// capability: i64 representing capability level (0=Immutable, 1=Read, 2=Write, 3=Owned)
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn init_global_identity_context(identity_ptr: i64, capability: i64) {
    if identity_ptr == 0 {
        // Null pointer - use default identity
        let default_identity = IdentityType::with_value(
            "anonymous".to_string(),
            vec![CapabilityLevel::from_i64(capability)],
        );
        GLOBAL_IDENTITY_CONTEXT = Some(IdentityContext::new(default_identity));
        return;
    }
    
    // Convert C string to Rust string
    let identity_str = match CStr::from_ptr(identity_ptr as *const c_char).to_str() {
        Ok(s) => s.to_string(),
        Err(_) => "anonymous".to_string(),
    };
    
    let identity = IdentityType::with_value(
        identity_str,
        vec![CapabilityLevel::from_i64(capability)],
    );
    
    GLOBAL_IDENTITY_CONTEXT = Some(IdentityContext::new(identity));
}

/// Get the global identity context.
///
/// # Safety
/// This function is not thread-safe.
unsafe fn get_global_context() -> Option<&'static mut IdentityContext> {
    // SAFETY: This is not thread-safe, but we're in a single-threaded context
    // for the runtime. The caller must ensure proper synchronization.
    // In Rust 2024, we need to use raw pointers to access mutable statics.
    let ptr = &raw mut GLOBAL_IDENTITY_CONTEXT as *mut Option<IdentityContext>;
    unsafe {
        (*ptr).as_mut()
    }
}

/// Identity-aware version of host_str_concat.
///
/// # Safety
/// Same as host_str_concat, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_concat(a: i64, b: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_concat(a, b);
        }
    };
    
    // Validate operation
    let a_str = if a != 0 {
        unsafe { CStr::from_ptr(a as *const c_char) }
            .to_str()
            .unwrap_or("")
    } else {
        ""
    };
    
    let b_str = if b != 0 {
        unsafe { CStr::from_ptr(b as *const c_char) }
            .to_str()
            .unwrap_or("")
    } else {
        ""
    };
    
    // Check capability for concat operation (requires Read)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Read) {
        // Log error and return null (0) to indicate failure
        eprintln!("[IDENTITY ERROR] host_str_concat: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_concat(a, b)
}

/// Identity-aware version of host_str_len.
///
/// # Safety
/// Same as host_str_len, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_len(s: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_len(s);
        }
    };
    
    // Validate operation (requires Read)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Read) {
        // Log error and return 0 (could also return -1 for error)
        eprintln!("[IDENTITY ERROR] host_str_len: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_len(s)
}

/// Identity-aware version of host_str_to_lowercase.
///
/// # Safety
/// Same as host_str_to_lowercase, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_to_lowercase(s: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_to_lowercase(s);
        }
    };
    
    // Validate operation (requires Write)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Write) {
        // Log error and return null (0) to indicate failure
        eprintln!("[IDENTITY ERROR] host_str_to_lowercase: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_to_lowercase(s)
}

/// Identity-aware version of host_str_to_uppercase.
///
/// # Safety
/// Same as host_str_to_uppercase, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_to_uppercase(s: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_to_uppercase(s);
        }
    };
    
    // Validate operation (requires Write)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Write) {
        // Log error and return null (0) to indicate failure
        eprintln!("[IDENTITY ERROR] host_str_to_uppercase: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_to_uppercase(s)
}

/// Identity-aware version of host_str_trim.
///
/// # Safety
/// Same as host_str_trim, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_trim(s: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_trim(s);
        }
    };
    
    // Validate operation (requires Write)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Write) {
        // Log error and return null (0) to indicate failure
        eprintln!("[IDENTITY ERROR] host_str_trim: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_trim(s)
}

/// Identity-aware version of host_str_starts_with.
///
/// # Safety
/// Same as host_str_starts_with, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_starts_with(haystack: i64, needle: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_starts_with(haystack, needle);
        }
    };
    
    // Validate operation (requires Read)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Read) {
        // Log error and return 0 (false) to indicate failure
        eprintln!("[IDENTITY ERROR] host_str_starts_with: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_starts_with(haystack, needle)
}

/// Identity-aware version of host_str_ends_with.
///
/// # Safety
/// Same as host_str_ends_with, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_ends_with(haystack: i64, needle: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_ends_with(haystack, needle);
        }
    };
    
    // Validate operation (requires Read)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Read) {
        // Log error and return 0 (false) to indicate failure
        eprintln!("[IDENTITY ERROR] host_str_ends_with: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_ends_with(haystack, needle)
}

/// Identity-aware version of host_str_contains.
///
/// # Safety
/// Same as host_str_contains, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_contains(haystack: i64, needle: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_contains(haystack, needle);
        }
    };
    
    // Validate operation (requires Read)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Read) {
        // Log error and return 0 (false) to indicate failure
        eprintln!("[IDENTITY ERROR] host_str_contains: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_contains(haystack, needle)
}

/// Identity-aware version of host_str_replace.
///
/// # Safety
/// Same as host_str_replace, plus requires global identity context to be initialized.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn identity_host_str_replace(s: i64, old: i64, new: i64) -> i64 {
    // Get global identity context
    let context = match get_global_context() {
        Some(ctx) => ctx,
        None => {
            // Identity context not initialized - fall back to regular function
            return crate::runtime::host::host_str_replace(s, old, new);
        }
    };
    
    // Validate operation (requires Write)
    if let Err(e) = context.validator().check_capability(CapabilityLevel::Write) {
        // Log error and return null (0) to indicate failure
        eprintln!("[IDENTITY ERROR] host_str_replace: {}", e);
        return 0;
    }
    
    // Call the original function
    crate::runtime::host::host_str_replace(s, old, new)
}

/// Helper function to create a C string from Rust string.
fn create_c_string(s: &str) -> i64 {
    match CString::new(s) {
        Ok(cstring) => {
            let len = cstring.as_bytes_with_nul().len();
            let ptr = unsafe { crate::runtime::std::std_malloc(len) };
            if ptr != 0 {
                unsafe {
                    ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
                }
            }
            ptr
        }
        Err(_) => 0,
    }
}

/// Reset the global identity context.
/// This is useful for testing to ensure clean state between tests.
///
/// # Safety
/// This function is not thread-safe.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn reset_global_identity_context() {
    GLOBAL_IDENTITY_CONTEXT = None;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::identity::{CapabilityLevel, IdentityType};
    
    #[test]
    fn test_identity_aware_functions() {
        // Initialize with read-only identity
        let identity_str = create_c_string("test");
        
        unsafe {
            init_global_identity_context(identity_str, 1); // 1 = Read capability
            
            // Create test strings
            let hello = create_c_string("hello");
            let world = create_c_string("world");
            
            // Read operations should work
            let len = identity_host_str_len(hello);
            assert!(len > 0);
            
            // Write operations should fail
            let result = identity_host_str_to_lowercase(hello);
            assert_eq!(result, 0); // Returns null (0) for failure
            
            // Clean up
            if hello != 0 {
                crate::runtime::std::std_free(hello as usize);
            }
            if world != 0 {
                crate::runtime::std::std_free(world as usize);
            }
            if identity_str != 0 {
                crate::runtime::std::std_free(identity_str as usize);
            }
        }
    }
    
    #[test]
    fn test_no_identity_context() {
        // Don't initialize context
        
        unsafe {
            // Create test string
            let hello = create_c_string("hello");
            
            // Without context, should fall back to regular function
            let len = identity_host_str_len(hello);
            assert!(len > 0);
            
            // Clean up
            if hello != 0 {
                crate::runtime::std::std_free(hello as usize);
            }
        }
    }
}