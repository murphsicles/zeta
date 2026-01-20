// src/runtime/host.rs
// Core host functions: string ops, http, tls, datetime, free
// Strings are owned UTF-8: i64 = malloc'd ptr to bytes + null terminator

use crate::runtime::std::{std_malloc, std_free};
use libc::strlen;
use reqwest::blocking::Client;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;
use std::time::{SystemTime, UNIX_EPOCH};

pub unsafe extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}

pub unsafe extern "C" fn host_free(ptr: *mut std::ffi::c_void) {
    if !ptr.is_null() {
        std_free(ptr as *mut u8);
    }
}

pub unsafe extern "C" fn host_http_get(url: *const c_char) -> i64 {
    if url.is_null() {
        return -1;
    }
    let url_str = match CStr::from_ptr(url).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };
    let client = Client::builder()
        .danger_accept_invalid_certs(true)
        .danger_accept_invalid_hostnames(true)
        .build()
        .unwrap_or_else(|_| Client::new());
    match client.get(url_str).send() {
        Ok(resp) => {
            if resp.status().is_success() {
                resp.content_length().map(|l| l as i64).unwrap_or(-2)
            } else {
                -3
            }
        }
        Err(_) => -4,
    }
}

pub unsafe extern "C" fn host_tls_handshake(_host: *const c_char) -> i64 {
    0 // success for demo
}

pub unsafe extern "C" fn host_str_concat(a: i64, b: i64) -> i64 {
    if a == 0 || b == 0 {
        return 0;
    }
    let s1 = CStr::from_ptr(a as *const c_char).to_str().unwrap_or("");
    let s2 = CStr::from_ptr(b as *const c_char).to_str().unwrap_or("");
    let concat = format!("{}{}", s1, s2);
    let cstring = CString::new(concat).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    ptr as i64
}

pub unsafe extern "C" fn host_str_len(s: i64) -> i64 {
    if s == 0 {
        0
    } else {
        strlen(s as *const c_char) as i64
    }
}

pub unsafe extern "C" fn host_str_to_lowercase(s: i64) -> i64 {
    string_op(s, |st| st.to_lowercase())
}

pub unsafe extern "C" fn host_str_to_uppercase(s: i64) -> i64 {
    string_op(s, |st| st.to_uppercase())
}

pub unsafe extern "C" fn host_str_trim(s: i64) -> i64 {
    string_op(s, |st| st.trim().to_string())
}

pub unsafe extern "C" fn host_str_starts_with(haystack: i64, needle: i64) -> i64 {
    string_pred(haystack, needle, |h, n| h.starts_with(n))
}

pub unsafe extern "C" fn host_str_ends_with(haystack: i64, needle: i64) -> i64 {
    string_pred(haystack, needle, |h, n| h.ends_with(n))
}

pub unsafe extern "C" fn host_str_contains(haystack: i64, needle: i64) -> i64 {
    string_pred(haystack, needle, |h, n| h.contains(n))
}

pub unsafe extern "C" fn host_str_replace(s: i64, old: i64, new: i64) -> i64 {
    if s == 0 || old == 0 || new == 0 {
        return 0;
    }
    let str_val = CStr::from_ptr(s as *const c_char).to_str().unwrap_or("");
    let old_val = CStr::from_ptr(old as *const c_char).to_str().unwrap_or("");
    let new_val = CStr::from_ptr(new as *const c_char).to_str().unwrap_or("");
    let replaced = str_val.replace(old_val, new_val);
    let cstring = CString::new(replaced).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    ptr as i64
}

unsafe fn string_op<F>(s: i64, op: F) -> i64
where
    F: FnOnce(&str) -> String,
{
    if s == 0 {
        return 0;
    }
    let input = CStr::from_ptr(s as *const c_char).to_str().unwrap_or("");
    let result = op(input);
    let cstring = CString::new(result).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    ptr as i64
}

unsafe fn string_pred<F>(haystack: i64, needle: i64, pred: F) -> i64
where
    F: FnOnce(&str, &str) -> bool,
{
    if haystack == 0 || needle == 0 {
        return 0;
    }
    let hay = CStr::from_ptr(haystack as *const c_char).to_str().unwrap_or("");
    let ndl = CStr::from_ptr(needle as *const c_char).to_str().unwrap_or("");
    if pred(hay, ndl) { 1 } else { 0 }
}
