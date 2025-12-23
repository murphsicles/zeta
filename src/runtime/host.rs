// src/runtime/host.rs
use std::time::{SystemTime, UNIX_EPOCH};
use std::ffi::c_char;
use std::ffi::CStr;
use std::os::raw::c_void;
use crate::runtime::std::free;

pub unsafe extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}

pub unsafe extern "C" fn host_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        free(ptr);
    }
}

pub unsafe extern "C" fn host_http_get(url: *const c_char) -> i64 {
    if let Ok(url_str) = CStr::from_ptr(url).to_str() {
        // Dummy impl: assume success.
        url_str.len() as i64
    } else {
        -1i64
    }
}

pub unsafe extern "C" fn host_tls_handshake(host: *const c_char) -> i64 {
    if CStr::from_ptr(host).to_str().is_ok() {
        0i64
    } else {
        -1i64
    }
}

pub unsafe extern "C" fn host_str_concat(a: *const u8, a_len: usize, b: *const u8, b_len: usize) -> *mut u8 {
    let mut result = Vec::with_capacity(a_len + b_len + 1);
    result.extend_from_slice(std::slice::from_raw_parts(a, a_len));
    result.extend_from_slice(std::slice::from_raw_parts(b, b_len));
    result.push(0);
    let ptr = result.as_mut_ptr();
    std::mem::forget(result);
    ptr
}
