// src/middle/resolver/resolver.rs
use crate::frontend::ast::AstNode;
use crate::frontend::borrow::BorrowChecker;
use crate::middle::mir::mir::Mir;
use crate::middle::specialization::{MonoKey, MonoValue, is_cache_safe, record_specialization};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

pub type Type = String;

/// Persistent cache file name
const SPECIALIZATION_CACHE_FILE: &str = ".zeta_specialization_cache.json";

#[derive(Serialize, Deserialize)]
struct CacheFile {
    entries: HashMap<MonoKey, MonoValue>,
}

pub struct Resolver {
    pub impls: HashMap<(String, String), Vec<AstNode>>,
    pub cached_mirs: HashMap<String, Mir>,
    pub mono_mirs: HashMap<MonoKey, Mir>,
    pub borrow_checker: RefCell<BorrowChecker>,
    pub associated_types: HashMap<(String, String), String>,
    /// New: CTFE constant cache (AstNode → i64)
    pub ctfe_consts: HashMap<AstNode, i64>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut r = Self {
            impls: HashMap::new(),
            cached_mirs: HashMap::new(),
            mono_mirs: HashMap::new(),
            borrow_checker: RefCell::new(BorrowChecker::new()),
            associated_types: HashMap::new(),
            ctfe_consts: HashMap::new(),
        };
        r.load_specialization_cache();
        r
    }

    /// Load persistent specialization cache from disk if exists
    fn load_specialization_cache(&mut self) {
        let path = PathBuf::from(SPECIALIZATION_CACHE_FILE);
        if let Ok(data) = fs::read_to_string(&path)
            && let Ok(cache) = serde_json::from_str::<CacheFile>(&data)
        {
            for (key, value) in cache.entries {
                // Insert placeholder MIR – actual MIR will be filled when generated
                self.mono_mirs.insert(
                    key.clone(),
                    Mir {
                        name: None,
                        param_indices: vec![],
                        stmts: vec![],
                        exprs: HashMap::new(),
                        ctfe_consts: HashMap::new(),
                        type_map: HashMap::new(),
                    },
                );
                record_specialization(key, value);
            }
        }
    }

    /// Save current specialization cache to disk
    pub fn persist_specialization_cache(&self) {
        let cache_guard = crate::middle::specialization::CACHE.read().unwrap();
        let entries: HashMap<MonoKey, MonoValue> = cache_guard.clone();

        let cache_file = CacheFile { entries };
        if let Ok(json) = serde_json::to_string_pretty(&cache_file) {
            let _ = fs::write(SPECIALIZATION_CACHE_FILE, json);
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        if let AstNode::ImplBlock {
            concept, ty, body, ..
        } = ast
        {
            self.impls.insert((concept, ty), body);
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Vec<AstNode>> {
        let key = (concept.to_string(), ty.to_string());
        self.impls.get(&key).cloned()
    }

    pub fn is_abi_stable(&self, key: &MonoKey) -> bool {
        key.type_args.iter().all(|t| is_cache_safe(t))
    }

    pub fn record_mono(&mut self, key: MonoKey, mir: Mir) {
        let cache_safe = self.is_abi_stable(&key);
        let mangled = key.mangle();

        record_specialization(
            key.clone(),
            MonoValue {
                llvm_func_name: mangled,
                cache_safe,
            },
        );

        self.mono_mirs.insert(key, mir);
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Resolver {
    fn drop(&mut self) {
        self.persist_specialization_cache();
    }
}
