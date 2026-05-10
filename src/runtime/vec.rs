//! Simple Vec runtime — heap-allocated dynamic array with i64 elements.
//!
//! Layout: [capacity: 8 bytes | len: 8 bytes | data: cap * 8 bytes]
//! All functions return/accept a data pointer (ptr to first element).

use crate::runtime::std::{std_free, std_malloc, std_realloc};
use std::ptr;

/// Create a new Vec with the given capacity.
/// Returns a pointer to the data area (first i64 element).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_new(capacity: i64) -> i64 {
    unsafe {
        let cap = if capacity < 4 { 4 } else { capacity };
        let total = 16 + (cap as usize) * 8;
        let raw = std_malloc(total);
        if raw == 0 {
            return 0;
        }
        unsafe {
            let base = raw as *mut u8;
            ptr::write_unaligned(base as *mut i64, cap);
            ptr::write_unaligned(base.add(8) as *mut i64, 0i64);
        }
        (raw as usize + 16) as i64
    }
}

/// Push a value onto the end of the Vec.
/// Returns the (possibly reallocated) data pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_push(ptr: i64, val: i64) -> i64 {
    unsafe {
        if ptr == 0 {
            let new_ptr = zeta_vec_new(4);
            return zeta_vec_push(new_ptr, val);
        }
        unsafe {
            let base = (ptr as usize - 16) as *mut u8;
            let cap = ptr::read_unaligned(base as *const i64);
            let len = ptr::read_unaligned(base.add(8) as *const i64);

            if len >= cap {
                let new_cap = if cap == 0 { 4 } else { cap * 2 };
                let old_total = 16 + (cap as usize) * 8;
                let new_total = 16 + (new_cap as usize) * 8;
                let new_base = std_realloc(base as i64, new_total as i64) as *mut u8;
                let new_data = (new_base as usize + 16) as i64;
                ptr::write_unaligned(new_base as *mut i64, new_cap);
                ptr::write_unaligned(new_base.add(8) as *mut i64, len);
                let offset = (len as usize) * 8;
                ptr::write_unaligned((new_data as usize + offset) as *mut i64, val);
                ptr::write_unaligned(new_base.add(8) as *mut i64, len + 1);
                return new_data;
            }

            let offset = (len as usize) * 8;
            ptr::write_unaligned((ptr as usize + offset) as *mut i64, val);
            ptr::write_unaligned(base.add(8) as *mut i64, len + 1);
        }
        ptr
    }
}

/// Pop a value from the end of the Vec.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_pop(ptr: i64) -> i64 {
    if ptr == 0 {
        return 0;
    }
    unsafe {
        let base = (ptr as usize - 16) as *mut u8;
        let len = ptr::read_unaligned(base.add(8) as *const i64);
        if len <= 0 {
            return 0;
        }
        let new_len = len - 1;
        let offset = (new_len as usize) * 8;
        let val = ptr::read_unaligned((ptr as usize + offset) as *const i64);
        ptr::write_unaligned(base.add(8) as *mut i64, new_len);
        val
    }
}

/// Get element at index (0-based). Returns 0 if out of bounds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_get(ptr: i64, index: i64) -> i64 {
    if ptr == 0 || index < 0 {
        return 0;
    }
    unsafe {
        let base = (ptr as usize - 16) as *const u8;
        let len = ptr::read_unaligned(base.add(8) as *const i64);
        if index >= len {
            return 0;
        }
        let offset = (index as usize) * 8;
        ptr::read_unaligned((ptr as usize + offset) as *const i64)
    }
}

/// Set element at index. Returns ptr (for chaining).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_set(ptr: i64, index: i64, val: i64) -> i64 {
    if ptr == 0 || index < 0 {
        return ptr;
    }
    unsafe {
        let base = (ptr as usize - 16) as *const u8;
        let len = ptr::read_unaligned(base.add(8) as *const i64);
        if index >= len {
            return ptr;
        }
        let offset = (index as usize) * 8;
        ptr::write_unaligned((ptr as usize + offset) as *mut i64, val);
    }
    ptr
}

/// Returns the length of the Vec.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_len(ptr: i64) -> i64 {
    if ptr == 0 {
        return 0;
    }
    unsafe {
        let base = (ptr as usize - 16) as *const u8;
        ptr::read_unaligned(base.add(8) as *const i64)
    }
}

/// Returns the capacity of the Vec.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_capacity(ptr: i64) -> i64 {
    if ptr == 0 {
        return 0;
    }
    unsafe {
        let base = (ptr as usize - 16) as *const u8;
        ptr::read_unaligned(base as *const i64)
    }
}

/// Clears the Vec (sets len to 0, keeps memory).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_clear(ptr: i64) {
    if ptr == 0 {
        return;
    }
    unsafe {
        let base = (ptr as usize - 16) as *mut u8;
        ptr::write_unaligned(base.add(8) as *mut i64, 0i64);
    }
}

/// Frees the Vec's memory.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn zeta_vec_free(ptr: i64) {
    if ptr == 0 {
        return;
    }
    let base = (ptr as usize - 16) as i64;
    unsafe {
        std_free(base as usize);
    }
}
