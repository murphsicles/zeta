// src/runtime/host.rs
use crate::runtime::std::std_free;
use reqwest::blocking::Client;
use std::ffi::{c_char, CStr, CString};
use std::os::raw::c_void;
use std::ptr;
use std::time::{SystemTime, UNIX_EPOCH};
use libc::strlen;

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
        std_free(ptr as *mut u8);
    }
}

/// Performs a real HTTP GET using reqwest and returns body length.
///
/// # Safety
/// The url must be a valid null-terminated C string.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_http_get(url: *const c_char) -> i64 {
    if url.is_null() {
        return -1;
    }

    let url_str = match CStr::from_ptr(url).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };

    let client = Client::builder()
        .use_rustls_tls()
        .danger_accept_invalid_hostnames(true)
        .danger_accept_invalid_certs(true) // for testing/demo; remove in production
        .build()
        .unwrap_or_else(|_| Client::new());

    let resp = match client.get(url_str).send() {
        Ok(r) => r,
        Err(_) => return -4,
    };

    if resp.status().is_success() {
        resp.content_length().map(|l| l as i64).unwrap_or(-2)
    } else {
        -3
    }
}

/// Performs a real TLS handshake using rustls (dummy success for now).
///
/// # Safety
/// The host must be a valid null-terminated C string.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_tls_handshake(host: *const c_char) -> i64 {
    // Real TLS is handled by reqwest + rustls; this remains a dummy for compatibility
    if host.is_null() {
        return -1;
    }

    let _host_str = match CStr::from_ptr(host).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };

    // Success – actual TLS happens in reqwest
    0
}

/// Concatenates two strings and returns null-terminated pointer.
///
/// # Safety
/// Pointers a and b must be valid null-terminated strings. Caller must eventually free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_concat(a: i64, b: i64) -> i64 {
    let a_ptr = a as *const u8;
    let b_ptr = b as *const u8;

    let a_str = CStr::from_ptr(a_ptr as *const c_char).to_str().unwrap();
    let b_str = CStr::from_ptr(b_ptr as *const c_char).to_str().unwrap();

    let concat = format!("{}{}", a_str, b_str);
    let cstring = CString::new(concat).unwrap();
    let bytes = cstring.into_bytes_with_nul();
    let len = bytes.len();
    let ptr = crate::runtime::std::std_malloc(len);
    ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, len);
    ptr as i64
}

#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_to_lowercase(s: i64) -> i64 {
    let ptr = s as *const u8;
    let cstr = CStr::from_ptr(ptr as *const c_char);
    let lowered = cstr.to_str().unwrap().to_lowercase();
    let cstring = CString::new(lowered).unwrap();
    let bytes = cstring.into_bytes_with_nul();
    let len = bytes.len();
    let new_ptr = crate::runtime::std::std_malloc(len);
    ptr::copy_nonoverlapping(bytes.as_ptr(), new_ptr, len);
    new_ptr as i64
}

#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_to_uppercase(s: i64) -> i64 {
    let ptr = s as *const u8;
    let cstr = CStr::from_ptr(ptr as *const c_char);
    let uppered = cstr.to_str().unwrap().to_uppercase();
    let cstring = CString::new(uppered).unwrap();
    let bytes = cstring.into_bytes_with_nul();
    let len = bytes.len();
    let new_ptr = crate::runtime::std::std_malloc(len);
    ptr::copy_nonoverlapping(bytes.as_ptr(), new_ptr, len);
    new_ptr as i64
}

#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_trim(s: i64) -> i64 {
    let ptr = s as *const u8;
    let cstr = CStr::from_ptr(ptr as *const c_char);
    let trimmed = cstr.to_str().unwrap().trim();
    let cstring = CString::new(trimmed).unwrap();
    let bytes = cstring.into_bytes_with_nul();
    let len = bytes.len();
    let new_ptr = crate::runtime::std::std_malloc(len);
    ptr::copy_nonoverlapping(bytes.as_ptr(), new_ptr, len);
    new_ptr as i64
}

#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_len(s: i64) -> i64 {
    let ptr = s as *const c_char;
    strlen(ptr) as i64
}

#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_starts_with(haystack: i64, needle: i64) -> i64 {
    let hay = CStr::from_ptr(haystack as *const c_char).to_str().unwrap();
    let ndl = CStr::from_ptr(needle as *const c_char).to_str().unwrap();
    if hay.starts_with(ndl) { 1 } else { 0 }
}

#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_ends_with(haystack: i64, needle: i64) -> i64 {
    let hay = CStr::from_ptr(haystack as *const c_char).to_str().unwrap();
    let ndl = CStr::from_ptr(needle as *const c_char).to_str().unwrap();
    if hay.ends_with(ndl) { 1 } else { 0 }
}

#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_contains(haystack: i64, needle: i64) -> i64 {
    let hay = CStr::from_ptr(haystack as *const c_char).to_str().unwrap();
    let ndl = CStr::from_ptr(needle as *const c_char).to_str().unwrap();
    if hay.contains(ndl) { 1 } else { 0 }
}

#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_replace(s: i64, old: i64, new: i64) -> i64 {
    let str_val = CStr::from_ptr(s as *const c_char).to_str().unwrap();
    let old_val = CStr::from_ptr(old as *const c_char).to_str().unwrap();
    let new_val = CStr::from_ptr(new as *const c_char).to_str().unwrap();
    let replaced = str_val.replace(old_val, new_val);
    let cstring = CString::new(replaced).unwrap();
    let bytes = cstring.into_bytes_with_nul();
    let len = bytes.len();
    let new_ptr = crate::runtime::std::std_malloc(len);
    ptr::copy_nonoverlapping(bytes.as_ptr(), new_ptr, len);
    new_ptr as i64
}
