// src/runtime/actor/map.rs
use std::collections::HashMap;
use std::ffi::c_void;
#[derive(Debug)]
struct MapInner {
    inner: HashMap<i64, i64>,
}
impl MapInner {
    fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }
    fn insert(&mut self, key: i64, val: i64) {
        self.inner.insert(key, val);
    }
    fn get(&self, key: &i64) -> Option<&i64> {
        self.inner.get(key)
    }
}
/// Host function to create a new map.
/// # Safety
/// No params.
pub unsafe extern "C" fn host_map_new() -> *mut c_void {
    Box::into_raw(Box::new(MapInner::new())) as *mut c_void
}
/// Host function to insert a key-value pair into a map.
/// # Safety
/// Pointer must be valid Map ptr.
pub unsafe extern "C" fn host_map_insert(ptr: *mut c_void, key: i64, val: i64) {
    if !ptr.is_null() {
        unsafe {
            (*(ptr as *mut MapInner)).insert(key, val);
        }
    }
}
/// Host function to get a value from a map by key.
/// # Safety
/// Pointer must be valid Map ptr.
pub unsafe extern "C" fn host_map_get(ptr: *const c_void, key: i64) -> i64 {
    if ptr.is_null() {
        0
    } else {
        unsafe { (*(ptr as *const MapInner)).get(&key).cloned().unwrap_or(0) }
    }
}
/// Host function to free a map pointer.
/// # Safety
/// Pointer must be valid from map_new, not freed before.
pub unsafe extern "C" fn host_map_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        unsafe {
            let _ = Box::from_raw(ptr as *mut MapInner);
        }
    }
}
