// src/std.rs
use std::ffi::c_void;

#[link(name = "c")]
extern "C" {
    fn free(ptr: *mut c_void);
}

/// Frees a string returned by http_get/tls_get
pub unsafe fn free(ptr: *mut u8) {
    if !ptr.is_null() {
        unsafe { free(ptr as *mut c_void) }
    }
}
