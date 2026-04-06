//! Runtime support for array operations using ArrayHeader API
//! 
//! ArrayHeader layout (32 bytes total):
//! - magic: u64 (8 bytes) - corruption detection
//! - capacity: usize (8 bytes)
//! - len: usize (8 bytes)
//! - canary: u64 (8 bytes) - overflow detection
//! 
//! Data follows immediately after the header.
//! array_new returns pointer to data (after header), not to header.
#![allow(unsafe_code)]

use std::alloc::{alloc, dealloc, Layout};
use std::ptr;

/// ArrayHeader structure for bulletproof memory system
#[repr(C)]
struct ArrayHeader {
    magic: u64,      // Corruption detection (0xARRAYMAGIC)
    capacity: usize, // Maximum number of elements
    len: usize,      // Current number of elements
    canary: u64,     // Overflow detection (0xDEADBEEF)
}

const ARRAY_HEADER_SIZE: usize = std::mem::size_of::<ArrayHeader>();
const MAGIC_VALUE: u64 = 0xCAFEBABE; // Simple magic value for testing
const CANARY_VALUE: u64 = 0xDEADBEEF;

/// Get header from data pointer
unsafe fn get_header(data_ptr: *mut i64) -> *mut ArrayHeader {
    (data_ptr as *mut u8).sub(ARRAY_HEADER_SIZE) as *mut ArrayHeader
}

/// Get data pointer from header
unsafe fn get_data(header: *mut ArrayHeader) -> *mut i64 {
    (header as *mut u8).add(ARRAY_HEADER_SIZE) as *mut i64
}

/// Check if header is valid
unsafe fn check_header(header: *const ArrayHeader) -> bool {
    if header.is_null() {
        return false;
    }
    
    // Check magic value
    unsafe { (*header).magic == MAGIC_VALUE }
}

/// Check if canary is intact
unsafe fn check_canary(header: *const ArrayHeader) -> bool {
    if header.is_null() {
        return false;
    }
    
    unsafe { (*header).canary == CANARY_VALUE }
}

/// Create a new array with given capacity
/// 
/// # Safety
/// Returns a pointer to data (after ArrayHeader), not to header
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_new(capacity: usize) -> i64 {
    println!("[ARRAY_NEW] Called with capacity = {}", capacity);
    if capacity == 0 {
        println!("[ARRAY_NEW] Returning 0 because capacity == 0");
        return 0;
    }
    
    // Calculate total size: header + data
    let elem_size = std::mem::size_of::<i64>();
    let data_size = capacity * elem_size;
    let total_size = ARRAY_HEADER_SIZE + data_size;
    println!("[ARRAY_NEW] elem_size = {}, data_size = {}, total_size = {}", elem_size, data_size, total_size);
    
    // Allocate single contiguous block for header + data
    let align = std::cmp::max(std::mem::align_of::<ArrayHeader>(), std::mem::align_of::<i64>());
    println!("[ARRAY_NEW] align = {}", align);
    let layout = match Layout::from_size_align(total_size, align) {
        Ok(layout) => {
            println!("[ARRAY_NEW] Layout created successfully");
            layout
        },
        Err(e) => {
            println!("[ARRAY_NEW] Layout error: {:?}", e);
            return 0;
        }
    };
    
    println!("[ARRAY_NEW] Allocating memory...");
    let block_ptr = unsafe { alloc(layout) } as *mut u8;
    println!("[ARRAY_NEW] block_ptr = {:?}", block_ptr);
    if block_ptr.is_null() {
        println!("[ARRAY_NEW] Allocation failed, returning 0");
        return 0;
    }
    
    // Initialize header
    let header_ptr = block_ptr as *mut ArrayHeader;
    unsafe {
        (*header_ptr).magic = MAGIC_VALUE;
        (*header_ptr).capacity = capacity;
        (*header_ptr).len = 0;
        (*header_ptr).canary = CANARY_VALUE;
    }
    
    // Get data pointer (after header)
    let data_ptr = unsafe { get_data(header_ptr) };
    println!("[ARRAY_NEW] header_ptr = {:?}, data_ptr = {:?}", header_ptr, data_ptr);
    
    // Initialize data to zero
    unsafe {
        ptr::write_bytes(data_ptr as *mut u8, 0, data_size);
    }
    
    // Return data pointer (not header pointer)
    let result = data_ptr as i64;
    println!("[ARRAY_NEW] Returning data pointer: {}", result);
    result
}

/// Get array length
/// 
/// # Safety
/// ptr must be a valid data pointer returned by array_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_len(ptr: i64) -> i64 {
    if ptr == 0 {
        return 0;
    }
    
    let data_ptr = ptr as *mut i64;
    let header = unsafe { get_header(data_ptr) };
    
    // Check header integrity
    if !unsafe { check_header(header) } {
        // Magic corrupted - memory corruption detected
        return -1;
    }
    
    // Check canary for overflow detection
    if !unsafe { check_canary(header) } {
        // Canary corrupted - buffer overflow detected
        return -2;
    }
    
    unsafe { (*header).len as i64 }
}

/// Get element at index
/// 
/// # Safety
/// ptr must be a valid data pointer returned by array_new, index must be < length
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_get(ptr: i64, index: i64) -> i64 {
    println!("[ARRAY_GET] Called with ptr = {}, index = {}", ptr, index);
    if ptr == 0 {
        return 0;
    }
    
    let data_ptr = ptr as *mut i64;
    let header = unsafe { get_header(data_ptr) };
    
    // Check header integrity
    if !unsafe { check_header(header) } {
        // Magic corrupted - memory corruption detected
        return -1;
    }
    
    // Check canary for overflow detection
    if !unsafe { check_canary(header) } {
        // Canary corrupted - buffer overflow detected
        return -2;
    }
    
    // Bounds checking
    let len = unsafe { (*header).len };
    if index < 0 || index as usize >= len {
        // Index out of bounds
        return 0;
    }
    
    unsafe { *data_ptr.offset(index as isize) }
}

/// Set element at index
/// 
/// # Safety
/// ptr must be a valid data pointer returned by array_new, index must be < length
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set(ptr: i64, index: i64, value: i64) {
    if ptr == 0 {
        return;
    }
    
    let data_ptr = ptr as *mut i64;
    let header = unsafe { get_header(data_ptr) };
    
    // Check header integrity
    if !unsafe { check_header(header) } {
        // Magic corrupted - memory corruption detected
        return;
    }
    
    // Check canary for overflow detection
    if !unsafe { check_canary(header) } {
        // Canary corrupted - buffer overflow detected
        return;
    }
    
    // Bounds checking
    let len = unsafe { (*header).len };
    if index < 0 || index as usize >= len {
        // Index out of bounds
        return;
    }
    
    unsafe { *data_ptr.offset(index as isize) = value; }
}

/// Push element to array
/// 
/// # Safety
/// ptr must be a valid data pointer returned by array_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_push(ptr: i64, value: i64) {
    if ptr == 0 {
        println!("[ARRAY_PUSH] Called with ptr = 0, returning early");
        return;
    }
    println!("[ARRAY_PUSH] Called with ptr = {}, value = {}", ptr, value);
    
    let data_ptr = ptr as *mut i64;
    let header = unsafe { get_header(data_ptr) };
    
    // Check header integrity
    if !unsafe { check_header(header) } {
        // Magic corrupted - memory corruption detected
        return;
    }
    
    // Check canary for overflow detection
    if !unsafe { check_canary(header) } {
        // Canary corrupted - buffer overflow detected
        return;
    }
    
    let len = unsafe { (*header).len };
    let capacity = unsafe { (*header).capacity };
    
    if len >= capacity {
        // Array is full
        return;
    }
    
    // Add element
    unsafe {
        *data_ptr.offset(len as isize) = value;
        (*header).len = len + 1;
    }
}

/// Free array
/// 
/// # Safety
/// ptr must be a valid data pointer returned by array_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_free(ptr: i64) {
    if ptr == 0 {
        return;
    }
    
    let data_ptr = ptr as *mut i64;
    let header = unsafe { get_header(data_ptr) };
    
    // Calculate total size: header + data
    let capacity = unsafe { (*header).capacity };
    let elem_size = std::mem::size_of::<i64>();
    let data_size = capacity * elem_size;
    let total_size = ARRAY_HEADER_SIZE + data_size;
    
    // Get the original allocation pointer (start of header)
    let block_ptr = header as *mut u8;
    
    // Deallocate the entire block
    let align = std::cmp::max(std::mem::align_of::<ArrayHeader>(), std::mem::align_of::<i64>());
    if let Ok(layout) = Layout::from_size_align(total_size, align) {
        unsafe { dealloc(block_ptr, layout) };
    }
}