// src/runtime/host.rs
use std::time::{SystemTime, UNIX_EPOCH};
use std::ffi::c_char;
use std::ffi::CStr;
use crate::std::std_free;

extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as i64
}

extern "C" fn host_free(ptr: *mut std::ffi::c_void) {
    unsafe { std_free(ptr as *mut u8) }
}

extern "C" fn host_http_get(url: *const c_char) -> i64 {
    if let Ok(url_str) = unsafe { CStr::from_ptr(url) }.to_str() {
        url_str.len() as i64
    } else {
        -1i64
    }
}

extern "C" fn host_tls_handshake(host: *const c_char) -> i64 {
    if unsafe { CStr::from_ptr(host) }.to_str().is_ok() {
        0i64
    } else {
        -1i64
    }
}

extern "C" fn host_str_concat(a: *const u8, a_len: usize, b: *const u8, b_len: usize) -> *mut u8 {
    let mut result = Vec::with_capacity(a_len + b_len + 1);
    result.extend_from_slice(unsafe { std::slice::from_raw_parts(a, a_len) });
    result.extend_from_slice(unsafe { std::slice::from_raw_parts(b, b_len) });
    result.push(0);
    let ptr = result.as_mut_ptr();
    std::mem::forget(result);
    ptr
}
