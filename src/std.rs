// src/std.rs
//! Zeta standard library embeddings.
//! Provides malloc/free wrappers for heap ops with ownership tracking.
//! Updated: Safety docs for libc interop.

use std::ffi::c_void;
use std::ptr;

unsafe extern "C" {
    fn malloc(size: usize) -> *mut c_void;
    fn free(ptr: *mut c_void);
}

/// Allocates memory using libc malloc.
///
/// # Safety
///
/// The caller must ensure that:
/// - The size is valid and doesn't cause integer overflow
/// - The returned pointer is freed using `std_free` to avoid memory leaks
/// - The pointer is not used after being freed
pub unsafe fn std_malloc(size: usize) -> *mut u8 {
    if size == 0 {
        ptr::null_mut()
    } else {
        // Cast the returned c_void pointer to a u8 pointer
        unsafe { malloc(size) as *mut u8 }
    }
}

/// Frees memory allocated by `std_malloc`.
///
/// # Safety
///
/// The caller must ensure that:
/// - The pointer was allocated by `std_malloc` or is null
/// - The pointer is not used after being freed (no use-after-free)
/// - The pointer is not freed more than once (no double-free)
pub unsafe fn std_free(ptr: *mut u8) {
    if !ptr.is_null() {
        // Cast the u8 pointer to a c_void pointer for free()
        unsafe { free(ptr as *mut c_void) }
    }
}
