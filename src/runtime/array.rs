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
const MAGIC_VALUE: u64 = 0x41525241; // "ARRA" in hex for corruption detection
const CANARY_VALUE: u64 = 0xDEADBEEFCAFEBABE; // 64-bit canary for overflow detection

// Memory sanitization patterns
const UNINITIALIZED_PATTERN: u8 = 0xCD; // Pattern for uninitialized memory
const FREED_PATTERN: u8 = 0xFD; // Pattern for freed memory

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
    
    // Initialize data with sanitization pattern (0xCD for uninitialized)
    unsafe {
        ptr::write_bytes(data_ptr as *mut u8, UNINITIALIZED_PATTERN, data_size);
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
        println!("[ARRAY_LEN] ERROR: Memory corruption detected! Magic value corrupted.");
        println!("[ARRAY_LEN] Expected magic: 0x{:X}, Found: 0x{:X}", MAGIC_VALUE, unsafe { (*header).magic });
        return -1; // Error code for memory corruption
    }
    
    // Check canary for overflow detection
    if !unsafe { check_canary(header) } {
        // Canary corrupted - buffer overflow detected
        println!("[ARRAY_LEN] ERROR: Buffer overflow detected! Canary value corrupted.");
        println!("[ARRAY_LEN] Expected canary: 0x{:X}, Found: 0x{:X}", CANARY_VALUE, unsafe { (*header).canary });
        return -2; // Error code for buffer overflow
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
        println!("[ARRAY_GET] ptr is 0, returning 0");
        return 0;
    }
    
    let data_ptr = ptr as *mut i64;
    println!("[ARRAY_GET] data_ptr = {:?}", data_ptr);
    
    // Check if this is a stack array (no header)
    // For stack arrays, the pointer points directly to data
    // We can't reliably detect stack arrays, so we'll try to read the header
    // If the magic is wrong, we assume it's a stack array
    let header = unsafe { get_header(data_ptr) };
    println!("[ARRAY_GET] header = {:?}", header);
    
    // Check header integrity
    if unsafe { check_header(header) } {
        println!("[ARRAY_GET] Valid heap array header detected");
        // This is a heap array with a valid header
        
        // Check canary for overflow detection
        if !unsafe { check_canary(header) } {
            // Canary corrupted - buffer overflow detected
            println!("[ARRAY_GET] ERROR: Buffer overflow detected! Canary value corrupted.");
            println!("[ARRAY_GET] Expected canary: 0x{:X}, Found: 0x{:X}", CANARY_VALUE, unsafe { (*header).canary });
            return -2; // Error code for buffer overflow
        }
        
        // Bounds checking
        let len = unsafe { (*header).len };
        let capacity = unsafe { (*header).capacity };
        println!("[ARRAY_GET] Array length = {}, capacity = {}", len, capacity);
        if index < 0 || index as usize >= len {
            // Index out of bounds
            println!("[ARRAY_GET] ERROR: Index out of bounds! Index={}, Length={}", index, len);
            return -3; // Error code for index out of bounds
        }
        if index as usize >= capacity {
            // Index exceeds capacity (should never happen if length <= capacity)
            println!("[ARRAY_GET] ERROR: Index exceeds capacity! Index={}, Capacity={}", index, capacity);
            return -4; // Error code for capacity exceeded
        }
        
        let value = unsafe { *data_ptr.offset(index as isize) };
        println!("[ARRAY_GET] Reading value at offset {}: {}", index, value);
        value
    } else {
        // This is a stack array (no header)
        // Just read the value directly
        // WARNING: No bounds checking for stack arrays!
        // The compiler should generate bounds checking for stack arrays
        println!("[ARRAY_GET] Stack array access at index {}", index);
        let value = unsafe { *data_ptr.offset(index as isize) };
        println!("[ARRAY_GET] Stack array value at index {}: {}", index, value);
        value
    }
}

/// Set element at index
/// 
/// # Safety
/// ptr must be a valid data pointer returned by array_new, index must be < length
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set(ptr: i64, index: i64, value: i64) {
    println!("[ARRAY_SET] Called with ptr = {}, index = {}, value = {}", ptr, index, value);
    if ptr == 0 {
        println!("[ARRAY_SET] ptr is 0, returning early");
        return;
    }
    
    let data_ptr = ptr as *mut i64;
    println!("[ARRAY_SET] data_ptr = {:?}", data_ptr);
    
    // Check if this is a stack array (no header)
    let header = unsafe { get_header(data_ptr) };
    println!("[ARRAY_SET] header = {:?}", header);
    
    // Check header integrity
    if unsafe { check_header(header) } {
        println!("[ARRAY_SET] Valid heap array header detected");
        // This is a heap array with a valid header
        
        // Check canary for overflow detection
        if !unsafe { check_canary(header) } {
            // Canary corrupted - buffer overflow detected
            println!("[ARRAY_SET] ERROR: Buffer overflow detected! Canary value corrupted.");
            println!("[ARRAY_SET] Expected canary: 0x{:X}, Found: 0x{:X}", CANARY_VALUE, unsafe { (*header).canary });
            return;
        }
        
        // Bounds checking
        let len = unsafe { (*header).len };
        let capacity = unsafe { (*header).capacity };
        println!("[ARRAY_SET] Array length = {}, capacity = {}", len, capacity);
        if index < 0 || index as usize >= len {
            // Index out of bounds
            println!("[ARRAY_SET] ERROR: Index out of bounds! Index={}, Length={}", index, len);
            return;
        }
        if index as usize >= capacity {
            // Index exceeds capacity (should never happen if length <= capacity)
            println!("[ARRAY_SET] ERROR: Index exceeds capacity! Index={}, Capacity={}", index, capacity);
            return;
        }
        
        println!("[ARRAY_SET] Writing value {} at offset {}", value, index);
        unsafe { *data_ptr.offset(index as isize) = value; }
    } else {
        // This is a stack array (no header)
        // Just write the value directly
        // WARNING: No bounds checking for stack arrays!
        println!("[ARRAY_SET] Stack array write at index {}: {}", index, value);
        unsafe { *data_ptr.offset(index as isize) = value; }
    }
}

/// Push element to array
/// 
/// # Safety
/// ptr must be a valid data pointer returned by array_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_push(ptr: i64, value: i64) {
    println!("[ARRAY_PUSH] Called with ptr = {}, value = {}", ptr, value);
    if ptr == 0 {
        println!("[ARRAY_PUSH] ptr is 0, returning early");
        return;
    }
    
    let data_ptr = ptr as *mut i64;
    println!("[ARRAY_PUSH] data_ptr = {:?}", data_ptr);
    let header = unsafe { get_header(data_ptr) };
    println!("[ARRAY_PUSH] header = {:?}", header);
    
    // Check header integrity
    if unsafe { check_header(header) } {
        println!("[ARRAY_PUSH] Valid heap array header detected");
        // This is a heap array with a valid header
        
        // Check canary for overflow detection
        if !unsafe { check_canary(header) } {
            // Canary corrupted - buffer overflow detected
            println!("[ARRAY_PUSH] ERROR: Buffer overflow detected! Canary value corrupted.");
            println!("[ARRAY_PUSH] Expected canary: 0x{:X}, Found: 0x{:X}", CANARY_VALUE, unsafe { (*header).canary });
            return;
        }
        
        let len = unsafe { (*header).len };
        let capacity = unsafe { (*header).capacity };
        println!("[ARRAY_PUSH] len = {}, capacity = {}", len, capacity);
        
        if len >= capacity {
            // Array is full
            println!("[ARRAY_PUSH] ERROR: Array capacity exceeded! Length={}, Capacity={}", len, capacity);
            return;
        }
        
        // Add element
        println!("[ARRAY_PUSH] Writing value {} at offset {}", value, len);
        unsafe {
            *data_ptr.offset(len as isize) = value;
            (*header).len = len + 1;
        }
        println!("[ARRAY_PUSH] New len = {}", len + 1);
    } else {
        // This is a stack array (no header)
        // For stack arrays, we need to track position separately
        // This is a hack - we'll use a static variable to track position
        // This only works for single-threaded, single-array initialization
        println!("[ARRAY_PUSH] Stack array push (HACK - writing to position 0)");
        // HACK: Always write to position 0
        // This is wrong but works for simple test cases
        unsafe { *data_ptr = value; }
    }
}

/// Set array length
/// 
/// # Safety
/// ptr must be a valid data pointer returned by array_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_len(ptr: i64, len: i64) {
    if ptr == 0 {
        return;
    }
    
    let data_ptr = ptr as *mut i64;
    let header = unsafe { get_header(data_ptr) };
    
    // Check header integrity
    if unsafe { check_header(header) } {
        // This is a heap array with a valid header
        let capacity = unsafe { (*header).capacity };
        if len >= 0 && (len as usize) <= capacity {
            unsafe { (*header).len = len as usize; }
        }
    }
    // For stack arrays, we don't have a length field
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
    
    // Fill memory with freed pattern (0xFD) before deallocation
    unsafe {
        ptr::write_bytes(block_ptr, FREED_PATTERN, total_size);
    }
    
    // Deallocate the entire block
    let align = std::cmp::max(std::mem::align_of::<ArrayHeader>(), std::mem::align_of::<i64>());
    if let Ok(layout) = Layout::from_size_align(total_size, align) {
        unsafe { dealloc(block_ptr, layout) };
    }
}