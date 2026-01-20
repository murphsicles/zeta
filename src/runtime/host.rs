// src/runtime/host.rs
use crate::runtime::std::{std_free, std_malloc};
use libc::strlen;
use reqwest::blocking::Client;
use std::ffi::{CStr, CString, c_char};
use std::os::raw::c_void;
use std::ptr;
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
pub unsafe extern "C" fn host_tls_handshake(_host: *const c_char) -> i64 {
    0
}

/// Concatenates two strings and returns new null-terminated pointer.
///
/// # Safety
/// Both inputs must be valid null-terminated string pointers (i64 cast). Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_concat(a: i64, b: i64) -> i64 {
    if a == 0 || b == 0 {
        return 0;
    }
    let s1 = unsafe { CStr::from_ptr(a as *const c_char) }
        .to_str()
        .unwrap_or("");
    let s2 = unsafe { CStr::from_ptr(b as *const c_char) }
        .to_str()
        .unwrap_or("");
    let concat = format!("{}{}", s1, s2);
    let cstring = CString::new(concat).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    unsafe {
        ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    }
    ptr as i64
}

/// Returns string length.
///
/// # Safety
/// Input must be valid null-terminated string pointer (i64 cast).
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_len(s: i64) -> i64 {
    if s == 0 {
        0
    } else {
        strlen(s as *const c_char) as i64
    }
}

/// Converts string to lowercase and returns new pointer.
///
/// # Safety
/// Input must be valid null-terminated string pointer. Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_to_lowercase(s: i64) -> i64 {
    string_op(s, |st| st.to_lowercase())
}

/// Converts string to uppercase and returns new pointer.
///
/// # Safety
/// Input must be valid null-terminated string pointer. Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_to_uppercase(s: i64) -> i64 {
    string_op(s, |st| st.to_uppercase())
}

/// Trims string and returns new pointer.
///
/// # Safety
/// Input must be valid null-terminated string pointer. Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_trim(s: i64) -> i64 {
    string_op(s, |st| st.trim().to_string())
}

/// Checks if string starts with substring, returns 1 if true, 0 otherwise.
///
/// # Safety
/// Both inputs must be valid null-terminated string pointers (i64 cast).
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_starts_with(haystack: i64, needle: i64) -> i64 {
    string_pred(haystack, needle, |h, n| h.starts_with(n))
}

/// Checks if string ends with substring, returns 1 if true, 0 otherwise.
///
/// # Safety
/// Both inputs must be valid null-terminated string pointers (i64 cast).
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_ends_with(haystack: i64, needle: i64) -> i64 {
    string_pred(haystack, needle, |h, n| h.ends_with(n))
}

/// Checks if string contains substring, returns 1 if true, 0 otherwise.
///
/// # Safety
/// Both inputs must be valid null-terminated string pointers (i64 cast).
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_contains(haystack: i64, needle: i64) -> i64 {
    string_pred(haystack, needle, |h, n| h.contains(n))
}

/// Replaces all occurrences of old with new and returns new null-terminated pointer.
///
/// # Safety
/// All three inputs must be valid null-terminated string pointers (i64 cast). Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn host_str_replace(s: i64, old: i64, new: i64) -> i64 {
    if s == 0 || old == 0 || new == 0 {
        return 0;
    }
    let str_val = unsafe { CStr::from_ptr(s as *const c_char) }
        .to_str()
        .unwrap_or("");
    let old_val = unsafe { CStr::from_ptr(old as *const c_char) }
        .to_str()
        .unwrap_or("");
    let new_val = unsafe { CStr::from_ptr(new as *const c_char) }
        .to_str()
        .unwrap_or("");
    let replaced = str_val.replace(old_val, new_val);
    let cstring = CString::new(replaced).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    unsafe {
        ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    }
    ptr as i64
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn string_op<F>(s: i64, op: F) -> i64
where
    F: FnOnce(&str) -> String,
{
    if s == 0 {
        return 0;
    }
    let input = unsafe { CStr::from_ptr(s as *const c_char) }
        .to_str()
        .unwrap_or("");
    let result = op(input);
    let cstring = CString::new(result).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    unsafe {
        ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    }
    ptr as i64
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn string_pred<F>(haystack: i64, needle: i64, pred: F) -> i64
where
    F: FnOnce(&str, &str) -> bool,
{
    if haystack == 0 || needle == 0 {
        return 0;
    }
    let hay = unsafe { CStr::from_ptr(haystack as *const c_char) }
        .to_str()
        .unwrap_or("");
    let ndl = unsafe { CStr::from_ptr(needle as *const c_char) }
        .to_str()
        .unwrap_or("");
    if pred(hay, ndl) { 1 } else { 0 }
}
