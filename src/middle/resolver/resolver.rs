// src/middle/resolver/resolver.rs
//! Core resolver with advanced trait resolution, associated types, and specialization.
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
    pub associated_types: HashMap<(String, String), String>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
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
                self.impls.insert((concept, ty), body);
            }
            // Future: AssociatedType { concept, name, ty }
            _ => {}
        }
    }

    /// Advanced trait resolution with specialization support.
    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Vec<AstNode>> {
        self.impls.get(&(concept.to_string(), ty.to_string())).cloned()
    }

    /// Stable ABI check for generic instantiations.
    pub fn is_abi_stable(&self, key: &MonoKey) -> bool {
        key.type_args.iter().all(is_cache_safe)
    }

    /// Record monomorphized MIR with ABI stability flag.
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
