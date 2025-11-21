// src/specialization.rs
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, RwLock};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct MonoKey {
    pub func_name: String,
    pub type_args: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MonoValue {
    pub llvm_func_name: String,
    pub cache_safe: bool,
}

lazy_static::lazy_static! {
    static ref SPECIALIZATION_CACHE: Arc<RwLock<HashMap<MonoKey, MonoValue>>> = Arc::new(RwLock::new(HashMap::new()));
}

pub trait CacheSafe: 'static {}
impl CacheSafe for i64 {}
impl CacheSafe for f32 {}
impl CacheSafe for bool {}
impl<T: CacheSafe> CacheSafe for *const T {}
impl<T: CacheSafe> CacheSafe for *mut T {}
impl<T: CacheSafe, const N: usize> CacheSafe for [T; N] {}

pub fn is_cache_safe(ty: &str) -> bool {
    matches!(ty, "i64" | "f32" | "bool" | "&i64" | "&mut i64" | "&f32" | "&mut f32")
        || ty.starts_with("[i64;") || ty.starts_with("[f32;")
}

pub fn record_specialization(key: MonoKey, value: MonoValue) {
    let mut cache = SPECIALIZATION_CACHE.write().unwrap();
    cache.insert(key, value);
}

pub fn lookup_specialization(key: &MonoKey) -> Option<MonoValue> {
    let cache = SPECIALIZATION_CACHE.read().unwrap();
    cache.get(key).cloned()
}
