//! Runtime support for Option type
#![allow(unsafe_code)]

use std::alloc::{Layout, alloc, dealloc};

/// Option enum representation:
/// - None: 0
/// - Some: 1 + data
#[repr(C)]
pub struct OptionPtr(*mut u8);

/// Create a Some value
///
/// # Safety
/// The returned pointer must be freed with `option_free` to avoid memory leaks.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_make_some(data: i64) -> *mut u8 {
    // Allocate space for tag (1 byte) + padding (7 bytes) + data (8 bytes)
    // Total 16 bytes with 8-byte alignment
    let layout = Layout::from_size_align(16, 8).unwrap();
    let ptr = unsafe { alloc(layout) };

    // Set tag to 1 (Some) at offset 0
    unsafe {
        *ptr = 1;
    }

    // Store data at offset 8 (8-byte aligned)
    let data_ptr = unsafe { ptr.add(8) as *mut i64 };
    unsafe {
        *data_ptr = data;
    }

    ptr
}

/// Create a None value
///
/// # Safety
/// The returned pointer must be freed with `option_free` to avoid memory leaks.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_make_none() -> *mut u8 {
    // Allocate space for tag only (1 byte)
    let layout = Layout::from_size_align(1, 1).unwrap();
    let ptr = unsafe { alloc(layout) };

    // Set tag to 0 (None)
    unsafe {
        *ptr = 0;
    }

    ptr
}

/// Check if an Option is Some
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_is_some(opt: *mut u8) -> i64 {
    if opt.is_null() {
        return 0;
    }

    let tag = unsafe { *opt };
    if tag == 1 { 1 } else { 0 }
}

/// Get data from a Some value
/// Safety: Only call on Some values
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_get_data(opt: *mut u8) -> i64 {
    if opt.is_null() {
        return 0;
    }

    let data_ptr = unsafe { opt.add(8) as *mut i64 };
    unsafe { *data_ptr }
}

/// Free an Option
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_free(opt: *mut u8) {
    if opt.is_null() {
        return;
    }

    let tag = unsafe { *opt };
    let size = if tag == 1 { 16 } else { 1 };
    let align = if tag == 1 { 8 } else { 1 };
    let layout = Layout::from_size_align(size, align).unwrap();
    unsafe {
        dealloc(opt, layout);
    }
}
