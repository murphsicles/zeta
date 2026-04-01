// src/runtime/std.rs
use std::ffi::c_void;
use std::ptr;

unsafe extern "C" {
    pub fn malloc(size: usize) -> *mut c_void;
    pub fn free(ptr: *mut c_void);
}

/// Allocates memory via libc malloc.
///
/// # Safety
/// Caller must ensure valid size, free returned pointer with std_free, and avoid use after free.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe fn std_malloc(size: usize) -> *mut u8 {
    if size == 0 {
        ptr::null_mut()
    } else {
        malloc(size) as *mut u8
    }
}

/// Frees memory allocated by std_malloc.
///
/// # Safety
/// Caller must ensure pointer from std_malloc or null, no use after free, and no double free.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe fn std_free(ptr: *mut u8) {
    if !ptr.is_null() {
        free(ptr as *mut c_void)
    }
}

/// Prints an integer to stdout.
///
/// # Safety
/// No safety concerns.
pub unsafe extern "C" fn std_print(value: i64) {
    print!("{}", value);
}

/// Prints an integer to stdout with newline.
///
/// # Safety
/// No safety concerns.
pub unsafe extern "C" fn std_println(value: i64) {
    println!("{}", value);
}

/// Gets command line arguments.
/// Returns a pointer to an array of string pointers.
/// For now, returns 0 (null) as a stub.
///
/// # Safety
/// No safety concerns.
pub unsafe extern "C" fn std_args() -> i64 {
    0
}
