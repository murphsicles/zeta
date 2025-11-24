// src/std.rs
//! Standard library embeddings for Zeta.
//! Provides low-level C interop for memory management and system calls.

use std::ffi::c_void;

/// Links to the C standard library's free function.
#[link(name = "c")]
unsafe extern "C" {
    fn free(ptr: *mut c_void);
}

/// Frees heap-allocated memory, Zeta's RAII defer equivalent.
/// 
/// # Safety
/// `ptr` must point to valid, unfreed heap memory (e.g., from malloc).
/// Undefined behavior if invalid or double-freed.
pub unsafe fn std_free(ptr: *mut u8) {
    if !ptr.is_null() {
        // Cast to void* for C free.
        unsafe { free(ptr as *mut c_void) }
    }
}
