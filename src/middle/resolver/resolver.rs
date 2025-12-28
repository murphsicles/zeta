// src/middle/resolver/resolver.rs
use crate::frontend::ast::AstNode;
use crate::frontend::borrow::BorrowChecker;
use crate::middle::mir::mir::Mir;
use crate::middle::specialization::{MonoKey, MonoValue, is_cache_safe, record_specialization};
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
