//! Runtime support for Option type

use std::alloc::{alloc, dealloc, Layout};
use std::ptr;

/// Option enum representation:
/// - None: 0
/// - Some: 1 + data
#[repr(C)]
pub struct OptionPtr(*mut u8);

/// Create a Some value
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_make_some(data: i64) -> OptionPtr {
    // Allocate space for tag (1 byte) + data (8 bytes)
    let layout = Layout::from_size_align(9, 8).unwrap();
    let ptr = unsafe { alloc(layout) as *mut u8 };
    
    // Set tag to 1 (Some)
    unsafe { *ptr = 1; }
    
    // Store data
    let data_ptr = unsafe { ptr.add(1) as *mut i64 };
    unsafe { *data_ptr = data; }
    
    OptionPtr(ptr)
}

/// Create a None value
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_make_none() -> OptionPtr {
    // Allocate space for tag only (1 byte)
    let layout = Layout::from_size_align(1, 1).unwrap();
    let ptr = unsafe { alloc(layout) as *mut u8 };
    
    // Set tag to 0 (None)
    unsafe { *ptr = 0; }
    
    OptionPtr(ptr)
}

/// Check if an Option is Some
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_is_some(opt: OptionPtr) -> i64 {
    if opt.0.is_null() {
        return 0;
    }
    
    let tag = unsafe { *opt.0 };
    if tag == 1 {
        1
    } else {
        0
    }
}

/// Get data from a Some value
/// Safety: Only call on Some values
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_get_data(opt: OptionPtr) -> i64 {
    if opt.0.is_null() {
        return 0;
    }
    
    let data_ptr = unsafe { opt.0.add(1) as *mut i64 };
    unsafe { *data_ptr }
}

/// Free an Option
#[unsafe(no_mangle)]
pub unsafe extern "C" fn option_free(opt: OptionPtr) {
    if opt.0.is_null() {
        return;
    }
    
    let tag = unsafe { *opt.0 };
    let size = if tag == 1 { 9 } else { 1 };
    let align = if tag == 1 { 8 } else { 1 };
    let layout = Layout::from_size_align(size, align).unwrap();
    unsafe { dealloc(opt.0, layout); }
}