//! Collections module for Zeta standard library.
//! 
//! Provides implementations of common collection types:
//! - Vec<T>: Dynamic array
//! - HashMap<K, V>: Hash table
//! - String: UTF-8 encoded string

use std::collections::HashMap as RustHashMap;

/// Initializes the collections module.
pub fn init() {
    // Initialize collection types and functions
    println!("Collections module initialized");
}

/// Registers collection functions with the runtime.
pub fn register_functions(map: &mut std::collections::HashMap<&'static str, usize>) {
    // Vec functions
    map.insert("vec_new", vec_new as *const () as usize);
    map.insert("vec_push", vec_push as *const () as usize);
    map.insert("vec_pop", vec_pop as *const () as usize);
    map.insert("vec_len", vec_len as *const () as usize);
    map.insert("vec_get", vec_get as *const () as usize);
    
    // HashMap functions
    map.insert("hashmap_new", hashmap_new as *const () as usize);
    map.insert("hashmap_insert", hashmap_insert as *const () as usize);
    map.insert("hashmap_get", hashmap_get as *const () as usize);
    map.insert("hashmap_remove", hashmap_remove as *const () as usize);
    map.insert("hashmap_len", hashmap_len as *const () as usize);
    
    // String functions
    map.insert("string_new", string_new as *const () as usize);
    map.insert("string_from", string_from as *const () as usize);
    map.insert("string_len", string_len as *const () as usize);
    map.insert("string_is_empty", string_is_empty as *const () as usize);
    map.insert("string_concat", string_concat as *const () as usize);
}

// ============================================================================
// Vec<T> Implementation
// ============================================================================

/// Vec structure for dynamic arrays
pub struct Vec<T> {
    data: *mut T,
    len: usize,
    capacity: usize,
}

/// Creates a new empty Vec.
/// 
/// # Safety
/// Returns a pointer to a Vec structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_new() -> *mut Vec<i32> {
    let vec = Box::new(Vec {
        data: std::ptr::null_mut(),
        len: 0,
        capacity: 0,
    });
    Box::into_raw(vec)
}

/// Pushes an element onto the Vec.
/// 
/// # Safety
/// vec must be a valid pointer from vec_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_push(vec: *mut Vec<i32>, value: i32) { unsafe {
    if let Some(vec) = vec.as_mut() {
        // Simple implementation - in real implementation would handle resizing
        if vec.len >= vec.capacity {
            // For now, just increment capacity
            vec.capacity = vec.capacity.max(1) * 2;
        }
        vec.len += 1;
    }
}}

/// Pops an element from the Vec.
/// 
/// # Safety
/// vec must be a valid pointer from vec_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_pop(vec: *mut Vec<i32>) -> i32 { unsafe {
    if let Some(vec) = vec.as_mut() {
        if vec.len > 0 {
            vec.len -= 1;
            // Return dummy value for now
            return 0;
        }
    }
    -1 // Error value
}}

/// Gets the length of the Vec.
/// 
/// # Safety
/// vec must be a valid pointer from vec_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_len(vec: *const Vec<i32>) -> usize { unsafe {
    if let Some(vec) = vec.as_ref() {
        vec.len
    } else {
        0
    }
}}

/// Gets an element from the Vec by index.
/// 
/// # Safety
/// vec must be a valid pointer from vec_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vec_get(vec: *const Vec<i32>, index: usize) -> i32 { unsafe {
    if let Some(vec) = vec.as_ref() {
        if index < vec.len {
            // Return dummy value for now
            return 0;
        }
    }
    -1 // Error value
}}

// ============================================================================
// HashMap<K, V> Implementation
// ============================================================================

/// HashMap structure for hash tables
pub struct HashMap<K, V> {
    data: RustHashMap<K, V>,
}

/// Creates a new empty HashMap.
/// 
/// # Safety
/// Returns a pointer to a HashMap structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hashmap_new() -> *mut HashMap<String, i32> {
    let map = Box::new(HashMap {
        data: RustHashMap::new(),
    });
    Box::into_raw(map)
}

/// Inserts a key-value pair into the HashMap.
/// 
/// # Safety
/// map must be a valid pointer from hashmap_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hashmap_insert(
    map: *mut HashMap<String, i32>,
    key_ptr: *const u8,
    key_len: usize,
    value: i32,
) -> bool { unsafe {
    if let Some(map) = map.as_mut() {
        // Convert raw pointer to String
        let key_bytes = std::slice::from_raw_parts(key_ptr, key_len);
        let key = String::from_utf8_lossy(key_bytes).to_string();
        map.data.insert(key, value);
        true
    } else {
        false
    }
}}

/// Gets a value from the HashMap by key.
/// 
/// # Safety
/// map must be a valid pointer from hashmap_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hashmap_get(
    map: *const HashMap<String, i32>,
    key_ptr: *const u8,
    key_len: usize,
) -> i32 { unsafe {
    if let Some(map) = map.as_ref() {
        let key_bytes = std::slice::from_raw_parts(key_ptr, key_len);
        let key = String::from_utf8_lossy(key_bytes).to_string();
        map.data.get(&key).copied().unwrap_or(-1)
    } else {
        -1
    }
}}

/// Removes a key-value pair from the HashMap.
/// 
/// # Safety
/// map must be a valid pointer from hashmap_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hashmap_remove(
    map: *mut HashMap<String, i32>,
    key_ptr: *const u8,
    key_len: usize,
) -> bool { unsafe {
    if let Some(map) = map.as_mut() {
        let key_bytes = std::slice::from_raw_parts(key_ptr, key_len);
        let key = String::from_utf8_lossy(key_bytes).to_string();
        map.data.remove(&key).is_some()
    } else {
        false
    }
}}

/// Gets the length of the HashMap.
/// 
/// # Safety
/// map must be a valid pointer from hashmap_new.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn hashmap_len(map: *const HashMap<String, i32>) -> usize { unsafe {
    if let Some(map) = map.as_ref() {
        map.data.len()
    } else {
        0
    }
}}

// ============================================================================
// String Implementation
// ============================================================================

/// String structure for UTF-8 strings
pub struct ZetaString {
    data: String,
}

/// Creates a new empty String.
/// 
/// # Safety
/// Returns a pointer to a ZetaString structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_new() -> *mut ZetaString {
    let s = Box::new(ZetaString {
        data: String::new(),
    });
    Box::into_raw(s)
}

/// Creates a String from a raw pointer.
/// 
/// # Safety
/// Returns a pointer to a ZetaString structure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_from(ptr: *const u8, len: usize) -> *mut ZetaString { unsafe {
    let bytes = std::slice::from_raw_parts(ptr, len);
    let s = Box::new(ZetaString {
        data: String::from_utf8_lossy(bytes).to_string(),
    });
    Box::into_raw(s)
}}

/// Gets the length of the String in bytes.
/// 
/// # Safety
/// s must be a valid pointer from string_new or string_from.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_len(s: *const ZetaString) -> usize { unsafe {
    if let Some(s) = s.as_ref() {
        s.data.len()
    } else {
        0
    }
}}

/// Checks if the String is empty.
/// 
/// # Safety
/// s must be a valid pointer from string_new or string_from.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_is_empty(s: *const ZetaString) -> bool { unsafe {
    if let Some(s) = s.as_ref() {
        s.data.is_empty()
    } else {
        true
    }
}}

/// Concatenates two strings.
/// 
/// # Safety
/// s1 and s2 must be valid pointers from string_new or string_from.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn string_concat(s1: *const ZetaString, s2: *const ZetaString) -> *mut ZetaString { unsafe {
    let empty = String::new();
    let s1_ref = s1.as_ref().map(|s| &s.data).unwrap_or(&empty);
    let s2_ref = s2.as_ref().map(|s| &s.data).unwrap_or(&empty);
    
    let result = Box::new(ZetaString {
        data: format!("{}{}", s1_ref, s2_ref),
    });
    Box::into_raw(result)
}}
