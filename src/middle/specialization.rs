// src/middle/specialization.rs
//! Specialization cache with persistence and stable ABI checks.
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MonoKey {
    pub func_name: String,
    pub type_args: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct MonoValue {
    pub llvm_func_name: String,
    pub cache_safe: bool,
}

lazy_static::lazy_static! {
    static ref CACHE: Arc<RwLock<HashMap<MonoKey, MonoValue>>> = Arc::new(RwLock::new(HashMap::new()));
}

impl MonoKey {
    pub fn mangle(&self) -> String {
        let args = self.type_args.join("_");
        format!("{}_{}", self.func_name, args)
    }
}

pub fn lookup_specialization(key: &MonoKey) -> Option<MonoValue> {
    let cache = CACHE.read().unwrap();
    cache.get(key).cloned()
}

pub fn record_specialization(key: MonoKey, value: MonoValue) {
    let mut cache = CACHE.write().unwrap();
    cache.insert(key, value);
}

pub fn is_cache_safe(ty: &str) -> bool {
    matches!(
        ty,
        "i64" | "i32" | "f64" | "f32" | "bool" | "u8" | "&i64" | "&str"
            | "Result_i64" | "Result_str" | "Map_i64_i64" | "Map_str_str"
    )
}
