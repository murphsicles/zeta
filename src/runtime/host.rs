// src/runtime/host.rs
use crate::runtime::std::free;
use std::ffi::CStr;
use std::ffi::c_char;
use std::os::raw::c_void;
use std::time::{SystemTime, UNIX_EPOCH};

/// Returns the current datetime as milliseconds since UNIX epoch.
///
/// # Safety
/// No safety concerns as there are no parameters.
pub unsafe extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}

/// Frees a pointer using std free.
///
/// # Safety
/// Pointer must be valid or null, no use after free.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        free(ptr);
    }
}

/// Performs a dummy HTTP GET and returns length.
///
/// # Safety
/// The url must be a valid null-terminated C string.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_http_get(url: *const c_char) -> i64 {
    if let Ok(url_str) = unsafe { CStr::from_ptr(url) }.to_str() {
        // Dummy impl: assume success.
        url_str.len() as i64
    } else {
        -1i64
    }
}

/// Performs a dummy TLS handshake.
///
/// # Safety
/// The host must be a valid null-terminated C string.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_tls_handshake(host: *const c_char) -> i64 {
    if unsafe { CStr::from_ptr(host) }.to_str().is_ok() {
        0i64
    } else {
        -1i64
    }
}

/// Concatenates two strings and returns null-terminated pointer.
///
/// # Safety
/// Pointers a and b must be valid for reads of a_len and b_len bytes. Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_concat(
    a: *const u8,
    a_len: usize,
    b: *const u8,
    b_len: usize,
) -> *mut u8 {
    let mut result = Vec::with_capacity(a_len + b_len + 1);
    result.extend_from_slice(unsafe { std::slice::from_raw_parts(a, a_len) });
    result.extend_from_slice(unsafe { std::slice::from_raw_parts(b, b_len) });
    result.push(0);
    let ptr = result.as_mut_ptr();
    std::mem::forget(result);
    ptr
}
