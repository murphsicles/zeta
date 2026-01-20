// src/runtime/std.rs
// Standard malloc/free wrappers around libc

use std::ffi::c_void;
use std::ptr;

unsafe extern "C" {
    pub fn malloc(size: usize) -> *mut c_void;
    pub fn free(ptr: *mut c_void);
}

pub unsafe fn std_malloc(size: usize) -> *mut u8 {
    if size == 0 {
        ptr::null_mut()
    } else {
        malloc(size) as *mut u8
    }
}

pub unsafe fn std_free(ptr: *mut u8) {
    if !ptr.is_null() {
        free(ptr as *mut c_void);
    }
}
