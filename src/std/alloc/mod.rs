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
pub unsafe extern "C" fn malloc<T>(count: usize) -> i64 {
    let size = mem::size_of::<T>() * count;
    unsafe { std::std_malloc(size) }
}

/// Allocates zero-initialized memory for `count` elements of type `T`.
/// 
/// # Safety
/// This function is unsafe because it returns a raw pointer.
/// The caller must ensure:
/// - The returned pointer is properly aligned for `T`
/// - The memory is freed with `free` when no longer needed
/// - No use-after-free or double-free occurs
pub unsafe extern "C" fn calloc<T>(count: usize) -> i64 {
    let size = mem::size_of::<T>() * count;
    let ptr = unsafe { std::std_malloc(size) };
    if ptr != 0 {
        // Convert to pointer and zero memory
        let ptr_mut = ptr as *mut u8;
        unsafe { ptr::write_bytes(ptr_mut, 0, size) };
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
pub unsafe extern "C" fn realloc<T>(ptr: i64, new_count: usize) -> i64 {
    let new_size = mem::size_of::<T>() * new_count;
    // For simplicity, we'll allocate new memory and copy
    // In a real implementation, this would use libc realloc
    let new_ptr = unsafe { std::std_malloc(new_size) };
    if new_ptr != 0 && ptr != 0 {
        let old_size = mem::size_of::<T>() * 1; // Simplified
        let copy_size = cmp::min(new_size, old_size);
        unsafe { ptr::copy_nonoverlapping(ptr as *const u8, new_ptr as *mut u8, copy_size) };
        unsafe { std::std_free(ptr as usize) };
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
pub unsafe extern "C" fn free<T>(ptr: i64) {
    unsafe {
        std::std_free(ptr as usize)
    }
}

/// Initializes the alloc module.
pub fn init() {
    // Nothing to initialize for now
}

/// Registers allocation functions in the standard library map.
pub fn register_functions(map: &mut HashMap<&'static str, usize>) {
    // Register non-generic versions for PrimeZeta compatibility
    map.insert("malloc", malloc_raw as *const () as usize);
    map.insert("free", free_raw as *const () as usize);
    
    // We'll need to handle generic instantiation separately
    // For now, we'll also register generic versions with type erasure
    map.insert("calloc", calloc::<u8> as *const () as usize);
    map.insert("realloc", realloc::<u8> as *const () as usize);
}

/// Non-generic malloc for PrimeZeta compatibility
/// Allocates raw memory without type information
/// 
/// # Safety
/// Same as generic malloc
pub unsafe extern "C" fn malloc_raw(size: usize) -> i64 {
    unsafe { std::std_malloc(size) }
}

/// Non-generic free for PrimeZeta compatibility
/// Frees raw memory without type information
/// 
/// # Safety
/// Same as generic free
pub unsafe extern "C" fn free_raw(ptr: i64) {
    unsafe { std::std_free(ptr as usize) }
}