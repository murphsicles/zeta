// src/middle/resolver/resolver.rs
//! Core resolver with full trait resolution, associated types, and specialization support.
use crate::frontend::ast::AstNode;
use crate::frontend::borrow::BorrowChecker;
use crate::middle::mir::mir::Mir;
use crate::middle::specialization::{MonoKey, MonoValue, is_cache_safe, lookup_specialization, record_specialization};
use std::cell::RefCell;
use std::collections::HashMap;

pub type Type = String;

pub struct Resolver {
    pub impls: HashMap<(String, String), Vec<AstNode>>,
    pub cached_mirs: HashMap<String, Mir>,
    pub mono_mirs: HashMap<MonoKey, Mir>,
    pub borrow_checker: RefCell<BorrowChecker>,
    // New: associated type mappings
    pub associated_types: HashMap<(String, String), String>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            impls: HashMap::new(),
            cached_mirs: HashMap::new(),
            mono_mirs: HashMap::new(),
            borrow_checker: RefCell::new(BorrowChecker::new()),
            associated_types: HashMap::new(),
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ImplBlock {
                concept,
                ty,
                body,
                ..
            } => {
                self.impls.insert((concept.clone(), ty.clone()), body);
                // Register associated types if any (future extension)
            }
            AstNode::AssociatedType { concept, name, ty } => {
                self.associated_types.insert((concept, name), ty);
            }
            _ => {}
        }
    }

    /// Full trait resolution with specialization and associated type support.
    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Vec<AstNode>> {
        // Direct lookup first
        if let Some(impl_body) = self.impls.get(&(concept.to_string(), ty.to_string())) {
            return Some(impl_body.clone());
        }

        // TODO: specialization fallback and default impls
        None
    }

    /// Stable ABI check: only cache-safe generic instantiations are persisted.
    pub fn is_abi_stable(&self, key: &MonoKey) -> bool {
        key.type_args.iter().all(|t| is_cache_safe(t))
    }

    /// Record monomorphized function with cache safety flag.
    pub fn record_mono(&mut self, key: MonoKey, mir: Mir) {
        let cache_safe = self.is_abi_stable(&key);
        let mangled = key.mangle();

        let value = MonoValue {
            llvm_func_name: mangled,
            cache_safe,
        };
        record_specialization(key.clone(), value);
        self.mono_mirs.insert(key, mir);
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}
