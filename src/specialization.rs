// src/specialization.rs
//! Specialization cache for thin monomorphization.
//! Manages LLVM func name mangling and cache safety for generic calls.
//! Thread-safe global cache via RwLock for concurrent access.

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// Key for monomorphization cache: function + type args.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MonoKey {
    /// Base function name.
    pub func_name: String,
    /// Type arguments for specialization.
    pub type_args: Vec<String>,
}

/// Value in monomorphization cache: mangled name + safety flag.
#[derive(Clone, Debug)]
pub struct MonoValue {
    /// Mangled LLVM function name.
    pub llvm_func_name: String,
    /// True if cache entry is safe for reuse across compilations.
    pub cache_safe: bool,
}

// Global thread-safe cache for specializations.
lazy_static::lazy_static! {
    static ref CACHE: Arc<RwLock<HashMap<MonoKey, MonoValue>>> = Arc::new(RwLock::new(HashMap::new()));
}

/// Looks up a specialization by key, returning cloned value if present.
pub fn lookup_specialization(key: &MonoKey) -> Option<MonoValue> {
    let cache = CACHE.read().unwrap();
    cache.get(key).cloned()
}

/// Records a new specialization in the cache.
pub fn record_specialization(key: MonoKey, value: MonoValue) {
    let mut cache = CACHE.write().unwrap();
    cache.insert(key, value);
}

/// Checks if a type arg is cache-safe (primitives/pointers only).
pub fn is_cache_safe(ty: &str) -> bool {
    matches!(ty, "i64" | "f32" | "bool" | "&i64" | "&mut i64")
}
