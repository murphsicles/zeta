// src/std.rs
use std::ffi::c_void;

#[link(name = "c")]
unsafe extern "C" {
    fn free(ptr: *mut c_void);
}

/// Frees the memory pointed to by `ptr`.
///
/// # Safety
/// `ptr` must be a valid pointer to heap-allocated memory (e.g., from `malloc`),
/// and it must not have been freed before.
pub unsafe fn std_free(ptr: *mut u8) {
    if !ptr.is_null() {
        unsafe { free(ptr as *mut c_void) }
    }
}
