// src/std.rs
//! Standard library embeddings for Zeta.
//! Provides low-level C interop for memory management and system calls.

use std::ffi::c_void;

// Links to the C standard library's free function.
#[link(name = "c")]
unsafe extern "C" {
    fn free(ptr: *mut c_void);
}

#[link(name = "c")]
unsafe extern "C" {
    fn malloc(size: usize) -> *mut c_void;
}

/// Allocates heap memory, Zeta's malloc equivalent.
pub unsafe fn std_free(ptr: *mut u8) {
    if size == 0 {
        std::ptr::null_mut()
    } else {
        unsafe { malloc(size) as *mut u8 }
    }
}

/// Frees heap-allocated memory, Zeta's RAII defer equivalent.
pub unsafe fn std_free(ptr: *mut u8) {
    if !ptr.is_null() {
        unsafe { free(ptr as *mut c_void) }
    }
}
