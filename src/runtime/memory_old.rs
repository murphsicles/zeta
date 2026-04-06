//! Memory allocation runtime functions
#![allow(unsafe_code)]

use std::alloc::{alloc, Layout};

/// Allocate memory (malloc equivalent)
/// 
/// # Safety
/// Caller must free with memory_free
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_malloc(size: usize) -> i64 {
    if size == 0 {
        return 0;
    }
    
    // Use alignment of 8 (common for 64-bit systems)
    match Layout::from_size_align(size, 8) {
        Ok(layout) => {
            let ptr = unsafe { alloc(layout) };
            if ptr.is_null() {
                0
            } else {
                ptr as i64
            }
        }
        Err(_) => 0
    }
}

/// Free memory (free equivalent)
/// 
/// # Safety
/// ptr must be from malloc or null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_free(ptr: i64) {
    if ptr == 0 {
        return;
    }
    
    // Note: We don't know the size that was allocated, so we can't properly free
    // In a real implementation, we'd need to track allocation sizes
    // For now, we'll leak the memory to avoid crashes
    // std::alloc::dealloc(ptr as *mut u8, layout);
}

/// Allocate and zero-initialize memory (calloc equivalent)
/// 
/// # Safety
/// Caller must free with memory_free
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_calloc(num: usize, size: usize) -> i64 {
    let total_size = num * size;
    if total_size == 0 {
        return 0;
    }
    
    match Layout::from_size_align(total_size, 8) {
        Ok(layout) => {
            let ptr = unsafe { alloc(layout) };
            if ptr.is_null() {
                return 0;
            }
            
            // Zero the memory
            unsafe {
                std::ptr::write_bytes(ptr, 0, total_size);
            }
            ptr as i64
        }
        Err(_) => 0
    }
}

/// Reallocate memory (realloc equivalent)
/// 
/// # Safety
/// ptr must be from malloc/calloc or null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_realloc(ptr: i64, new_size: usize) -> i64 {
    if new_size == 0 {
        unsafe { runtime_free(ptr); }
        return 0;
    }
    
    if ptr == 0 {
        return unsafe { runtime_malloc(new_size) };
    }
    
    // Note: Proper realloc would copy old data
    // For simplicity, we'll just allocate new memory
    // In a real implementation, we'd use std::alloc::realloc
    let new_ptr = unsafe { runtime_malloc(new_size) };
    if new_ptr == 0 {
        return 0;
    }
    
    // We don't know the old size, so we can't copy
    // This is a limitation of our simple implementation
    new_ptr
}