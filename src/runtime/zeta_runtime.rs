//! Zeta runtime support module
//!
//! Runtime functions that are called from compiled Zeta code.
//! These delegate to the proper runtime implementations.

/// Runtime context
pub struct RuntimeContext {
    // Placeholder for future state
}

impl RuntimeContext {
    /// Create new runtime context
    pub fn new() -> Self {
        Self {}
    }
}

/// Get bool from array — delegates to array runtime
#[allow(unused_variables)]
pub unsafe extern "C" fn zeta_array_get_bool(array_ptr: i64, index: i64) -> i64 {
    // SAFETY: caller must ensure valid pointer and index
    unsafe { crate::runtime::array::array_get(array_ptr, index) }
}

/// Get i64 from array — delegates to array runtime
#[allow(unused_variables)]
pub unsafe extern "C" fn zeta_array_get_i64(array_ptr: i64, index: i64) -> i64 {
    // SAFETY: caller must ensure valid pointer and index
    unsafe { crate::runtime::array::array_get(array_ptr, index) }
}

/// Set boolean in array — delegates to array runtime
#[allow(unused_variables)]
pub unsafe extern "C" fn zeta_array_set_bool(array_ptr: i64, index: i64, value: i64) {
    // SAFETY: caller must ensure valid pointer and index
    unsafe {
        crate::runtime::array::array_set(array_ptr, index, value);
    }
}

/// Set i64 in array — delegates to array runtime
#[allow(unused_variables)]
pub unsafe extern "C" fn zeta_array_set_i64(array_ptr: i64, index: i64, value: i64) {
    // SAFETY: caller must ensure valid pointer and index
    unsafe {
        crate::runtime::array::array_set(array_ptr, index, value);
    }
}

/// Print i64
#[allow(unused_variables)]
pub unsafe extern "C" fn zeta_print_i64(value: i64) {
    print!("{}", value);
}

/// Print i64 with newline
#[allow(unused_variables)]
pub unsafe extern "C" fn zeta_println_i64(value: i64) {
    println!("{}", value);
}

/// Create a new array (sieve-compatible)
#[allow(unused_variables)]
pub unsafe extern "C" fn zeta_sieve_new(size: i64) -> i64 {
    // SAFETY: caller must ensure valid size
    unsafe { crate::runtime::array::array_new(size as usize) }
}
