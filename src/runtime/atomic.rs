//! Atomic runtime functions for Zeta std::sync::atomic
//!
//! Uses Rust's std::sync::atomic behind a simple pointer interface.
//! Each atomic allocates a boxed AtomicXxx on the heap and returns its pointer.

use std::sync::atomic::{AtomicBool, AtomicI32, AtomicI64, Ordering};

const SEQ: Ordering = Ordering::SeqCst;

// === AtomicBool ===

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_bool_new(val: i64) -> i64 {
    Box::into_raw(Box::new(AtomicBool::new(val != 0))) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_bool_load(ptr: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicBool)).load(SEQ) as i64 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_bool_store(ptr: i64, val: i64) {
    if ptr == 0 { return; }
    unsafe { (* (ptr as *mut AtomicBool)).store(val != 0, SEQ); }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_bool_swap(ptr: i64, val: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicBool)).swap(val != 0, SEQ) as i64 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_bool_cas(ptr: i64, current: i64, new: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicBool)).compare_exchange(current != 0, new != 0, SEQ, SEQ).unwrap_or(false) as i64 }
}

// === AtomicI64 ===

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_new(val: i64) -> i64 {
    Box::into_raw(Box::new(AtomicI64::new(val))) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_load(ptr: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI64)).load(SEQ) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_store(ptr: i64, val: i64) {
    if ptr == 0 { return; }
    unsafe { (* (ptr as *mut AtomicI64)).store(val, SEQ); }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_swap(ptr: i64, val: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI64)).swap(val, SEQ) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_cas(ptr: i64, current: i64, new: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI64)).compare_exchange(current, new, SEQ, SEQ).unwrap_or(current) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_fetch_add(ptr: i64, val: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI64)).fetch_add(val, SEQ) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_fetch_sub(ptr: i64, val: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI64)).fetch_sub(val, SEQ) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_fetch_and(ptr: i64, val: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI64)).fetch_and(val, SEQ) as i64 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_fetch_or(ptr: i64, val: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI64)).fetch_or(val, SEQ) as i64 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i64_fetch_xor(ptr: i64, val: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI64)).fetch_xor(val, SEQ) as i64 }
}

// === AtomicI32 ===

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i32_new(val: i64) -> i64 {
    Box::into_raw(Box::new(AtomicI32::new(val as i32))) as i64
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i32_load(ptr: i64) -> i64 {
    if ptr == 0 { return 0; }
    unsafe { (* (ptr as *mut AtomicI32)).load(SEQ) as i64 }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn atomic_i32_store(ptr: i64, val: i64) {
    if ptr == 0 { return; }
    unsafe { (* (ptr as *mut AtomicI32)).store(val as i32, SEQ); }
}
