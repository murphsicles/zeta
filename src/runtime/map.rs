//! Map runtime functions
#![allow(unsafe_code)]

use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::LazyLock;

// Simple global map store
static MAP_STORE: LazyLock<Mutex<HashMap<i64, HashMap<i64, i64>>>> = LazyLock::new(|| Mutex::new(HashMap::new()));
static NEXT_MAP_ID: AtomicI64 = AtomicI64::new(1);

/// Create a new map
/// 
/// # Safety
/// Returns pointer to map (actually a map ID)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn map_new() -> i64 {
    let id = NEXT_MAP_ID.fetch_add(1, Ordering::SeqCst);
    let mut store = MAP_STORE.lock().unwrap();
    store.insert(id, HashMap::new());
    id
}

/// Insert key-value pair into map
/// 
/// # Safety
/// ptr must be valid map ID from map_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn map_insert(ptr: i64, key: i64, value: i64) {
    let mut store = MAP_STORE.lock().unwrap();
    if let Some(map) = store.get_mut(&ptr) {
        map.insert(key, value);
    }
}

/// Get value from map by key
/// 
/// # Safety
/// ptr must be valid map ID from map_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn map_get(ptr: i64, key: i64) -> i64 {
    let store = MAP_STORE.lock().unwrap();
    if let Some(map) = store.get(&ptr) {
        *map.get(&key).unwrap_or(&0)
    } else {
        0
    }
}

/// Free map
/// 
/// # Safety
/// ptr must be valid map ID from map_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn map_free(ptr: i64) {
    let mut store = MAP_STORE.lock().unwrap();
    store.remove(&ptr);
}