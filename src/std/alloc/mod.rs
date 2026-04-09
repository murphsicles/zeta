//! Memory allocation module for Zeta standard library.
//! Provides malloc, free, calloc, and realloc functions.

use crate::runtime::std;
use core::mem;
use core::cmp;
use core::ptr;
use ::std::collections::HashMap;

/// Allocates memory for `count` elements of type `T`.
/// 
/// # Safety
/// This function is unsafe because it returns a raw pointer.
/// The caller must ensure:
/// - The returned pointer is properly aligned for `T`
/// - The memory is freed with `free` when no longer needed
/// - No use-after-free or double-free occurs
pub unsafe extern "C" fn malloc<T>(count: usize) -> *mut T {
    let size = mem::size_of::<T>() * count;
    unsafe { std::std_malloc(size) as *mut T }
}

/// Allocates zero-initialized memory for `count` elements of type `T`.
/// 
/// # Safety
/// This function is unsafe because it returns a raw pointer.
/// The caller must ensure:
/// - The returned pointer is properly aligned for `T`
/// - The memory is freed with `free` when no longer needed
/// - No use-after-free or double-free occurs
pub unsafe extern "C" fn calloc<T>(count: usize) -> *mut T {
    let size = mem::size_of::<T>() * count;
    let ptr = unsafe { std::std_malloc(size) as *mut T };
    if !ptr.is_null() {
        unsafe { ptr::write_bytes(ptr as *mut u8, 0, size) };
    }
    ptr
}

/// Reallocates memory previously allocated with `malloc` or `calloc`.
/// 
/// # Safety
/// This function is unsafe because:
/// - `ptr` must be a pointer previously returned by `malloc` or `calloc`
/// - `old_count` must be the original allocation size
/// - The memory region must not be used after reallocation
pub unsafe extern "C" fn realloc<T>(ptr: *mut T, new_count: usize) -> *mut T {
    let new_size = mem::size_of::<T>() * new_count;
    // For simplicity, we'll allocate new memory and copy
    // In a real implementation, this would use libc realloc
    let new_ptr = unsafe { std::std_malloc(new_size) as *mut T };
    if !new_ptr.is_null() && !ptr.is_null() {
        let old_size = mem::size_of::<T>() * 1; // Simplified
        let copy_size = cmp::min(new_size, old_size);
        unsafe { ptr::copy_nonoverlapping(ptr as *const u8, new_ptr as *mut u8, copy_size) };
        unsafe { std::std_free(ptr as *mut u8 as usize) };
    }
    new_ptr
}

/// Frees memory allocated by `malloc`, `calloc`, or `realloc`.
/// 
/// # Safety
/// This function is unsafe because:
/// - `ptr` must be a pointer previously returned by allocation functions
/// - The memory must not be used after this call
/// - No double-free should occur
pub unsafe extern "C" fn free<T>(ptr: *mut T) {
    unsafe {
        std::std_free(ptr as *mut u8 as usize)
    }
}

/// Initializes the alloc module.
pub fn init() {
    // Nothing to initialize for now
}

/// Registers allocation functions in the standard library map.
pub fn register_functions(map: &mut HashMap<&'static str, usize>) {
    // Note: These are generic functions, so we need special handling
    // For now, we'll register the non-generic versions
    map.insert("malloc", std::std_malloc as *const () as usize);
    map.insert("free", std::std_free as *const () as usize);
    
    // We'll need to handle generic instantiation separately
}