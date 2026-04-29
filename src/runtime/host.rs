// src/runtime/host.rs
use crate::runtime::std::{std_free, std_malloc};
use libc::strlen;
use reqwest::blocking::Client;
use std::ffi::{CStr, CString, c_char};
use std::os::raw::c_void;
use std::ptr;
use std::time::{SystemTime, UNIX_EPOCH};

/// Allocates memory using std_malloc.
///
/// # Safety
/// Caller must ensure valid size and free with host_free.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn runtime_malloc(size: usize) -> i64 {
    std_malloc(size)
}

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
        std_free(ptr as usize);
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
    ptr
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
    ptr
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
    ptr
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

/// Clone an i64 value (identity function for i64)
///
/// # Safety
/// No safety concerns as it just returns the input value.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn clone_i64(value: i64) -> i64 {
    value
}

/// Check if an i64 value is null (0)
///
/// # Safety
/// No safety concerns as it just compares the value to 0.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn is_null_i64(value: i64) -> i64 {
    if value == 0 { 1 } else { 0 }
}

/// Convert a string to a string (identity function for strings)
///
/// # Safety
/// Input must be a valid null-terminated string pointer or 0.
/// Caller must free returned pointer if not null.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn to_string_str(s: i64) -> i64 {
    if s == 0 {
        return 0;
    }

    // For strings, to_string is an identity operation
    // But we need to clone the string to follow ownership semantics
    let input = unsafe { CStr::from_ptr(s as *const c_char) }
        .to_str()
        .unwrap_or("");

    let cstring = CString::new(input).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    unsafe {
        ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    }
    ptr
}

/// Clone a boolean value (identity function for bool as i64)
///
/// # Safety
/// No safety concerns as it just returns the input value.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn clone_bool(value: i64) -> i64 {
    value
}

/// Check if a boolean value is null (false, which is 0)
///
/// # Safety
/// No safety concerns as it just checks if value is 0.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn is_null_bool(value: i64) -> i64 {
    if value == 0 { 1 } else { 0 }
}

/// Convert an i64 to a string
///
/// # Safety
/// Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn to_string_i64(value: i64) -> i64 {
    let string = value.to_string();
    let cstring = CString::new(string).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    unsafe {
        ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    }
    ptr
}

/// Convert a boolean to a string ("true" or "false")
///
/// # Safety
/// Caller must free returned pointer.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn to_string_bool(value: i64) -> i64 {
    let string = if value != 0 { "true" } else { "false" };
    let cstring = CString::new(string).unwrap();
    let len = cstring.as_bytes_with_nul().len();
    let ptr = std_malloc(len);
    unsafe {
        ptr::copy_nonoverlapping(cstring.as_ptr(), ptr as *mut c_char, len);
    }
    ptr
}

// ===== Arithmetic Runtime Functions =====

/// Add two i64 values
///
/// # Safety
/// No safety concerns as it just performs arithmetic.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn add_i64(a: i64, b: i64) -> i64 {
    a.wrapping_add(b)
}

/// Subtract two i64 values
///
/// # Safety
/// No safety concerns as it just performs arithmetic.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn sub_i64(a: i64, b: i64) -> i64 {
    a.wrapping_sub(b)
}

/// Multiply two i64 values
///
/// # Safety
/// No safety concerns as it just performs arithmetic.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn mul_i64(a: i64, b: i64) -> i64 {
    a.wrapping_mul(b)
}

/// Divide two i64 values
///
/// # Safety
/// Division by zero will panic.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn div_i64(a: i64, b: i64) -> i64 {
    if b == 0 {
        panic!("division by zero");
    }
    a.wrapping_div(b)
}

/// Modulo of two i64 values
///
/// # Safety
/// Division by zero will panic.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn mod_i64(a: i64, b: i64) -> i64 {
    if b == 0 {
        panic!("division by zero");
    }
    a.wrapping_rem(b)
}

// ===== Bitwise Runtime Functions =====

/// Left shift i64 value
///
/// # Safety
/// No safety concerns as it just performs bitwise operation.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn shl_i64(a: i64, b: i64) -> i64 {
    a.wrapping_shl(b as u32)
}

/// Right shift i64 value
///
/// # Safety
/// No safety concerns as it just performs bitwise operation.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn shr_i64(a: i64, b: i64) -> i64 {
    a.wrapping_shr(b as u32)
}

/// Bitwise AND of two i64 values
///
/// # Safety
/// No safety concerns as it just performs bitwise operation.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn and_i64(a: i64, b: i64) -> i64 {
    a & b
}

/// Bitwise OR of two i64 values
///
/// # Safety
/// No safety concerns as it just performs bitwise operation.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn or_i64(a: i64, b: i64) -> i64 {
    a | b
}

/// Bitwise XOR of two i64 values
///
/// # Safety
/// No safety concerns as it just performs bitwise operation.
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn xor_i64(a: i64, b: i64) -> i64 {
    a ^ b
}

// ===== Comparison Operators =====

/// Equality comparison for i64 values
///
/// # Safety
/// No safety concerns as it just compares two i64 values.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn eq_i64(a: i64, b: i64) -> i64 {
    if a == b { 1 } else { 0 }
}

/// Inequality comparison for i64 values
///
/// # Safety
/// No safety concerns as it just compares two i64 values.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn ne_i64(a: i64, b: i64) -> i64 {
    if a != b { 1 } else { 0 }
}

/// Less than comparison for i64 values
///
/// # Safety
/// No safety concerns as it just compares two i64 values.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn lt_i64(a: i64, b: i64) -> i64 {
    if a < b { 1 } else { 0 }
}

/// Greater than comparison for i64 values
///
/// # Safety
/// No safety concerns as it just compares two i64 values.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn gt_i64(a: i64, b: i64) -> i64 {
    if a > b { 1 } else { 0 }
}

/// Less than or equal comparison for i64 values
///
/// # Safety
/// No safety concerns as it just compares two i64 values.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn le_i64(a: i64, b: i64) -> i64 {
    if a <= b { 1 } else { 0 }
}

/// Greater than or equal comparison for i64 values
///
/// # Safety
/// No safety concerns as it just compares two i64 values.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn ge_i64(a: i64, b: i64) -> i64 {
    if a >= b { 1 } else { 0 }
}

/// Equality operator (==) - calls eq_i64
///
/// # Safety
/// No safety concerns as it just calls eq_i64.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn eq(a: i64, b: i64) -> i64 {
    eq_i64(a, b)
}

/// Inequality operator (!=) - calls ne_i64
///
/// # Safety
/// No safety concerns as it just calls ne_i64.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn ne(a: i64, b: i64) -> i64 {
    ne_i64(a, b)
}

/// Less than operator (<) - calls lt_i64
///
/// # Safety
/// No safety concerns as it just calls lt_i64.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn lt(a: i64, b: i64) -> i64 {
    lt_i64(a, b)
}

/// Greater than operator (>) - calls gt_i64
///
/// # Safety
/// No safety concerns as it just calls gt_i64.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn gt(a: i64, b: i64) -> i64 {
    gt_i64(a, b)
}

/// Less than or equal operator (<=) - calls le_i64
///
/// # Safety
/// No safety concerns as it just calls le_i64.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn le(a: i64, b: i64) -> i64 {
    le_i64(a, b)
}

/// Greater than or equal operator (>=) - calls ge_i64
///
/// # Safety
/// No safety concerns as it just calls ge_i64.
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn ge(a: i64, b: i64) -> i64 {
    ge_i64(a, b)
}

// ===== Dynamic Array Runtime Functions =====
// COMMENTED OUT: Duplicate functions moved to array.rs for bulletproof implementation
/*
/// Dynamic array structure
struct DynamicArray {
    capacity: i64,
    length: i64,
    data: *mut i64,
}

/// Creates a new dynamic array
///
/// # Safety
/// Returns a pointer to a heap-allocated DynamicArray
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn array_new_OLD_DUPLICATE() -> i64 {
    let arr = Box::new(DynamicArray {
        capacity: 0,
        length: 0,
        data: ptr::null_mut(),
    });
    Box::into_raw(arr) as i64
}

/// Pushes a value to a dynamic array
///
/// # Safety
/// arr_ptr must be a valid pointer to a DynamicArray
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn array_push(arr_ptr: i64, value: i64) {
    if arr_ptr == 0 {
        return;
    }
    let arr = &mut *(arr_ptr as *mut DynamicArray);

    // Resize if needed
    if arr.length >= arr.capacity {
        let new_capacity = if arr.capacity == 0 { 4 } else { arr.capacity * 2 };
        let new_size = (new_capacity as usize) * std::mem::size_of::<i64>();
        let new_data = std_malloc(new_size) as *mut i64;

        // Check if allocation succeeded
        if new_data.is_null() {
            // Allocation failed - cannot resize
            return;
        }

        if !arr.data.is_null() {
            // Copy existing data
            ptr::copy_nonoverlapping(arr.data, new_data, arr.length as usize);
            // Free old data (allocation tracker handles size lookup)
            crate::runtime::std::std_free(arr.data as usize);
        }

        arr.data = new_data;
        arr.capacity = new_capacity;
    }

    // Add new element
    unsafe {
        *arr.data.offset(arr.length as isize) = value;
    }
    arr.length += 1;
}

/// Gets the length of a dynamic array
///
/// # Safety
/// arr_ptr must be a valid pointer to a DynamicArray or 0
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn array_len(arr_ptr: i64) -> i64 {
    if arr_ptr == 0 {
        return 0;
    }
    let arr = &*(arr_ptr as *const DynamicArray);
    arr.length
}

/// Gets an element from a dynamic array by index
///
/// # Safety
/// arr_ptr must be a valid pointer to a DynamicArray
/// Returns 0 if index is out of bounds
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn array_get(arr_ptr: i64, index: i64) -> i64 {
    if arr_ptr == 0 {
        return 0;
    }
    let arr = &*(arr_ptr as *const DynamicArray);
    if index < 0 || index >= arr.length {
        return 0;
    }
    unsafe {
        *arr.data.offset(index as isize)
    }
}

/// Sets an element in a dynamic array by index
///
/// # Safety
/// arr_ptr must be a valid pointer to a DynamicArray
/// Index must be within bounds (0 <= index < length)
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn array_set(arr_ptr: i64, index: i64, value: i64) {
    if arr_ptr == 0 {
        return;
    }
    let arr = &mut *(arr_ptr as *mut DynamicArray);
    if index < 0 || index >= arr.length {
        return;
    }
    unsafe {
        *arr.data.offset(index as isize) = value;
    }
}

/// Frees a dynamic array
///
/// # Safety
/// arr_ptr must be a valid pointer to a DynamicArray or 0
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn array_free(arr_ptr: i64) {
    if arr_ptr == 0 {
        return;
    }
    let arr = Box::from_raw(arr_ptr as *mut DynamicArray);
    if !arr.data.is_null() {
        std_free(arr.data as usize);
    }
    // Box is dropped here, freeing the DynamicArray struct
}

/// Append i64 value to array (alias for array_push)
///
/// # Safety
/// Same as array_push
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn append_i64(arr_ptr: i64, value: i64) {
    array_push(arr_ptr, value)
}

/// Append u8 value to array
///
/// # Safety
/// Same as array_push
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn append_u8(arr_ptr: i64, value: i64) {
    // Convert i64 to u8 and push
    let u8_value = value as u8;
    array_push(arr_ptr, u8_value as i64)
}

/// Generic append function for stack arrays
///
/// # Safety
/// For stack arrays, we need special handling
#[unsafe(no_mangle)]
#[allow(unsafe_op_in_unsafe_fn)]
pub unsafe extern "C" fn append_stack_array(arr_ptr: i64, value: i64) -> i64 {
    // For now, just call array_push
    array_push(arr_ptr, value);
    0 // Return 0 for success
}
*/
