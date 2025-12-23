// src/runtime/std.rs
use std::ffi::c_void;
use std::ptr;

unsafe extern "C" {
    pub fn malloc(size: usize) -> *mut c_void;
    pub fn free(ptr: *mut c_void);
}
/// Allocates memory via libc malloc.
///
/* # Safety
Caller must ensure valid size, free returned pointer with std_free, and avoid use after free. */
pub unsafe fn std_malloc(size: usize) -> *mut u8 {
    if size == 0 {
        ptr::null_mut()
    } else {
        // Cast the returned c_void pointer to a u8 pointer
        malloc(size) as *mut u8
    }
}
/// Frees memory allocated by std_malloc.
///
/* # Safety
Caller must ensure pointer from std_malloc or null, no use after free, and no double free. */
pub unsafe fn std_free(ptr: *mut u8) {
    if !ptr.is_null() {
        // Cast the u8 pointer to a c_void pointer for free()
        free(ptr as *mut c_void)
    }
}
