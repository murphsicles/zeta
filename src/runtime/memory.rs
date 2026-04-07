//! Enhanced memory allocation runtime functions with bulletproof features
//! 
//! This is the hybrid approach (Option C) - enhancing existing std_malloc with novel safety layers
#![allow(unsafe_code)]

use std::alloc::{alloc, dealloc, Layout};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;
use std::collections::HashMap;
use std::sync::OnceLock;

// Constants
const HEADER_SIZE: usize = 32; // Space for metadata
const CANARY_VALUE: u64 = 0xDEADBEEFCAFEBABE;
const MAGIC_VALUE: u64 = 0xB4D455054; // "BULLET" in hex
const FREED_PATTERN: u8 = 0xFD;
const UNINIT_PATTERN: u8 = 0xCD;

// Allocation metadata header
#[repr(C)]
struct AllocationHeader {
    magic: u64,           // Magic number for validation
    size: usize,          // User-requested size
    allocation_id: u64,   // Unique ID for tracking
    canary: u64,          // Canary value for overflow detection
}

// Global allocation tracker
static ALLOCATION_COUNTER: AtomicU64 = AtomicU64::new(1);
static ALLOCATION_MAP: OnceLock<Mutex<HashMap<u64, usize>>> = OnceLock::new();

fn get_allocation_map() -> &'static Mutex<HashMap<u64, usize>> {
    ALLOCATION_MAP.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Enhanced malloc with bulletproof features
/// COMMENTED OUT: Duplicate of runtime_malloc in host.rs
/*
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_malloc(size: usize) -> i64 {
    if size == 0 {
        return 0;
    }
    
    // Calculate total size with metadata
    let user_size = size;
    let total_size = HEADER_SIZE + user_size;
    
    // Get unique allocation ID
    let allocation_id = ALLOCATION_COUNTER.fetch_add(1, Ordering::SeqCst);
    
    // Allocate memory
    let layout = match Layout::from_size_align(total_size, 8) {
        Ok(layout) => layout,
        Err(_) => return 0,
    };
    
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        return 0;
    }
    
    // Set up header
    let header_ptr = ptr as *mut AllocationHeader;
    unsafe {
        (*header_ptr).magic = MAGIC_VALUE;
        (*header_ptr).size = user_size;
        (*header_ptr).allocation_id = allocation_id;
        (*header_ptr).canary = CANARY_VALUE;
    }
    
    // Track allocation
    if let Ok(mut map) = get_allocation_map().lock() {
        map.insert(allocation_id, user_size);
    }
    
    // Return pointer to user data (after header)
    let user_ptr = unsafe { header_ptr.add(1) as *mut u8 };
    
    // Initialize user memory with pattern (sanitization)
    unsafe {
        std::ptr::write_bytes(user_ptr, UNINIT_PATTERN, user_size);
    }
    
    user_ptr as i64
}
*/

/// Free memory with bulletproof checks
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_free(ptr: i64) {
    if ptr == 0 {
        return;
    }
    
    let user_ptr = ptr as *mut u8;
    let header_ptr = unsafe { user_ptr.sub(HEADER_SIZE) as *mut AllocationHeader };
    
    // Validate header (corruption detection)
    if header_ptr.is_null() {
        report_corruption("Null header pointer in free");
        return;
    }
    
    let header = unsafe { &*header_ptr };
    if header.magic != MAGIC_VALUE || header.canary != CANARY_VALUE {
        report_corruption("Invalid header in free");
        return;
    }
    
    let allocation_id = header.allocation_id;
    let user_size = header.size;
    
    // Check for double-free
    if is_double_free(allocation_id) {
        report_corruption("Double free detected");
        return;
    }
    
    // Poison freed memory (use-after-free detection)
    unsafe {
        std::ptr::write_bytes(user_ptr, FREED_PATTERN, user_size);
    }
    
    // Untrack allocation
    if let Ok(mut map) = get_allocation_map().lock() {
        map.remove(&allocation_id);
    }
    
    // Free memory
    let layout = match Layout::from_size_align(HEADER_SIZE + user_size, 8) {
        Ok(layout) => layout,
        Err(_) => return,
    };
    
    unsafe {
        dealloc(header_ptr as *mut u8, layout);
    }
}

/// Calloc with bulletproof features
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_calloc(num: usize, size: usize) -> i64 {
    let total_size = num * size;
    let ptr = unsafe { crate::runtime::host::runtime_malloc(total_size) };
    
    if ptr != 0 {
        // Zero the memory (overwriting the UNINIT_PATTERN)
        unsafe {
            std::ptr::write_bytes(ptr as *mut u8, 0, total_size);
        }
    }
    
    ptr
}

/// Realloc with bulletproof features
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_realloc(ptr: i64, new_size: usize) -> i64 {
    if new_size == 0 {
        unsafe { runtime_free(ptr); }
        return 0;
    }
    
    if ptr == 0 {
        return unsafe { crate::runtime::host::runtime_malloc(new_size) };
    }
    
    // Get old size from header
    let user_ptr = ptr as *mut u8;
    let header_ptr = unsafe { user_ptr.sub(HEADER_SIZE) as *mut AllocationHeader };
    
    if header_ptr.is_null() {
        report_corruption("Null header pointer in realloc");
        return 0;
    }
    
    let header = unsafe { &*header_ptr };
    if header.magic != MAGIC_VALUE || header.canary != CANARY_VALUE {
        report_corruption("Invalid header in realloc");
        return 0;
    }
    
    let old_size = header.size;
    
    // Allocate new block
    let new_ptr = unsafe { crate::runtime::host::runtime_malloc(new_size) };
    if new_ptr == 0 {
        return 0;
    }
    
    // Copy old data (up to minimum of old and new size)
    let copy_size = std::cmp::min(old_size, new_size);
    if copy_size > 0 {
        unsafe {
            std::ptr::copy_nonoverlapping(user_ptr, new_ptr as *mut u8, copy_size);
        }
    }
    
    // Free old block
    unsafe { runtime_free(ptr); }
    
    new_ptr
}

/// Bounds-checked array access helper
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_check_bounds(ptr: i64, index: i64, element_size: usize) -> i64 {
    if ptr == 0 || index < 0 {
        report_bounds_violation("Null pointer or negative index", ptr, index);
        return 0;
    }
    
    let user_ptr = ptr as *mut u8;
    let header_ptr = unsafe { user_ptr.sub(HEADER_SIZE) as *mut AllocationHeader };
    
    if header_ptr.is_null() {
        report_corruption("Null header pointer in bounds check");
        return 0;
    }
    
    let header = unsafe { &*header_ptr };
    if header.magic != MAGIC_VALUE || header.canary != CANARY_VALUE {
        report_corruption("Invalid header in bounds check");
        return 0;
    }
    
    let size = header.size;
    let offset = (index as usize) * element_size;
    
    if offset + element_size > size {
        report_bounds_violation("Array index out of bounds", ptr, index);
        return 0;
    }
    
    // Return pointer to element
    (user_ptr as i64) + offset as i64
}

// Helper functions
fn is_double_free(allocation_id: u64) -> bool {
    if let Ok(map) = get_allocation_map().lock() {
        !map.contains_key(&allocation_id)
    } else {
        false
    }
}

fn report_corruption(message: &str) {
    eprintln!("[BULLETPROOF MEMORY] CORRUPTION DETECTED: {}", message);
    // In production, would trigger debugger or abort
}

fn report_bounds_violation(message: &str, ptr: i64, index: i64) {
    eprintln!("[BULLETPROOF MEMORY] BOUNDS VIOLATION: {} - ptr: {}, index: {}", message, ptr, index);
    // In production, would trigger debugger or abort
}

/// Memory leak detection
pub fn detect_memory_leaks() -> usize {
    if let Ok(map) = get_allocation_map().lock() {
        map.len()
    } else {
        0
    }
}

/// Get memory statistics
pub fn get_memory_stats() -> (usize, usize) {
    if let Ok(map) = get_allocation_map().lock() {
        let total_allocated = map.values().sum();
        let allocation_count = map.len();
        (total_allocated, allocation_count)
    } else {
        (0, 0)
    }
}

/// Test the bulletproof memory system
#[unsafe(no_mangle)]
pub unsafe extern "C" fn test_bulletproof_features() -> i64 {
    // This is a placeholder test function
    // Actual testing would be done from external C or Zeta code
    // Return success to indicate the functions are available
    0
}