// src/middle/specialization.rs
//! Manages specialization cache for monomorphization.
//! Handles LLVM function name mangling and cache safety for generics.
//! Uses thread-safe global cache with RwLock.
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
/// Key for monomorphization entries: function name and type arguments.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MonoKey {
    /// Base function name.
    pub func_name: String,
    /// Type arguments for specialization.
    pub type_args: Vec<String>,
}
/// Value for monomorphization entries: mangled name and safety flag.
#[derive(Clone, Debug)]
pub struct MonoValue {
    /// Mangled LLVM function name.
    pub llvm_func_name: String,
    /// Flag indicating if entry is safe for reuse across compilations.
    pub cache_safe: bool,
}
// Global thread-safe cache for specializations.
lazy_static::lazy_static! {
    static ref CACHE: Arc<RwLock<HashMap<MonoKey, MonoValue>>> = Arc::new(RwLock::new(HashMap::new()));
}
impl MonoKey {
    /// Generates mangled name from key (e.g., foo_i64_str).
    pub fn mangle(&self) -> String {
        let args = self.type_args.join(",");
        format!("{}_{}", self.func_name, args)
    }
}
/// Retrieves specialization by key, returning cloned value if found.
pub fn lookup_specialization(key: &MonoKey) -> Option<MonoValue> {
    let cache = CACHE.read().unwrap();
    cache.get(key).cloned()
}
/// Inserts new specialization into cache.
pub fn record_specialization(key: MonoKey, value: MonoValue) {
    let mut cache = CACHE.write().unwrap();
    cache.insert(key, value);
}
/// Determines if type argument is cache-safe (primitives or pointers).
pub fn is_cache_safe(ty: &str) -> bool {
    matches!(
        ty,
        "i64" | "f32" | "bool" | "&i64" | "&mut i64" | "str" | "&str"
    )
}
