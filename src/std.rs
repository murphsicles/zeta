// src/std.rs
use std::ffi::c_void;

#[link(name = "c")]
extern "C" {
    fn free(ptr: *mut c_void);
}

pub unsafe fn std_free(ptr: *mut u8) {
    if !ptr.is_null() {
        unsafe { free(ptr as *mut c_void) }
    }
}
