// src/runtime/actor/result.rs
use std::ffi::c_void;
#[derive(Debug)]
struct ResultInner {
    tag: bool, // true for Ok, false for Err
    data: i64,
}
/// Host function to create an ok result.
/// # Safety
/// Pointer must be valid from make_ok/err.
pub unsafe extern "C" fn host_result_make_ok(data: i64) -> *mut c_void {
    Box::into_raw(Box::new(ResultInner { tag: true, data })) as *mut c_void
}
/// Host function to create an error result.
/// # Safety
/// Pointer must be valid from make_ok/err.
pub unsafe extern "C" fn host_result_make_err(data: i64) -> *mut c_void {
    Box::into_raw(Box::new(ResultInner { tag: false, data })) as *mut c_void
}
/// Host function to check if a result is ok.
/// # Safety
/// Pointer must be valid Result ptr.
pub unsafe extern "C" fn host_result_is_ok(ptr: *const c_void) -> i64 {
    if ptr.is_null() {
        0
    } else {
        unsafe { (*(ptr as *const ResultInner)).tag as i64 }
    }
}
/// Host function to retrieve data from a result.
/// # Safety
/// Pointer must be valid Result ptr.
pub unsafe extern "C" fn host_result_get_data(ptr: *const c_void) -> i64 {
    if ptr.is_null() {
        0
    } else {
        unsafe { (*(ptr as *const ResultInner)).data }
    }
}
/// Host function to free a result pointer.
/// # Safety
/// Pointer must be valid from make_ok/err, not freed before.
pub unsafe extern "C" fn host_result_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        unsafe {
            let _ = Box::from_raw(ptr as *mut ResultInner);
        }
    }
}
