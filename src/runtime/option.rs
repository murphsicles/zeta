//! Runtime support for Option type

use std::alloc::{alloc, dealloc, Layout};

/// Option enum representation:
/// - None: 0
/// - Some: 1 + data
#[repr(C)]
pub struct OptionPtr(*mut u8);

/// Create a Some value
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_make_some(data: i64) -> *mut u8 {
    // Allocate space for tag (1 byte) + data (8 bytes)
    let layout = Layout::from_size_align(9, 8).unwrap();
    let ptr = unsafe { alloc(layout) as *mut u8 };
    
    // Set tag to 1 (Some)
    unsafe { *ptr = 1; }
    
    // Store data
    let data_ptr = unsafe { ptr.add(1) as *mut i64 };
    unsafe { *data_ptr = data; }
    
    ptr
}

/// Create a None value
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_make_none() -> *mut u8 {
    // Allocate space for tag only (1 byte)
    let layout = Layout::from_size_align(1, 1).unwrap();
    let ptr = unsafe { alloc(layout) as *mut u8 };
    
    // Set tag to 0 (None)
    unsafe { *ptr = 0; }
    
    ptr
}

/// Check if an Option is Some
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_is_some(opt: *mut u8) -> i64 {
    if opt.is_null() {
        return 0;
    }
    
    let tag = unsafe { *opt };
    if tag == 1 {
        1
    } else {
        0
    }
}

/// Get data from a Some value
/// Safety: Only call on Some values
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_get_data(opt: *mut u8) -> i64 {
    if opt.is_null() {
        return 0;
    }
    
    let data_ptr = unsafe { opt.add(1) as *mut i64 };
    unsafe { *data_ptr }
}

/// Free an Option
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_free(opt: *mut u8) {
    if opt.is_null() {
        return;
    }
    
    let tag = unsafe { *opt };
    let size = if tag == 1 { 9 } else { 1 };
    let align = if tag == 1 { 8 } else { 1 };
    let layout = Layout::from_size_align(size, align).unwrap();
    unsafe { dealloc(opt, layout); }
}