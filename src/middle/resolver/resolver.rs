// src/middle/resolver/resolver.rs
//! Core resolver structure and basic registration for Zeta concepts and implementations.
//! Integrates borrow checker for affine types.
use crate::frontend::ast::AstNode;
use crate::frontend::borrow::BorrowChecker;
use crate::middle::mir::mir::Mir;
use crate::middle::specialization::MonoKey;
use std::collections::HashMap;

pub type Type = String;

pub struct Resolver {
    pub impls: HashMap<(String, String), Vec<AstNode>>,
    pub cached_mirs: HashMap<String, Mir>,
    pub mono_mirs: HashMap<MonoKey, Mir>,
    pub borrow_checker: BorrowChecker,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            impls: HashMap::new(),
            cached_mirs: HashMap::new(),
            mono_mirs: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        if let AstNode::ImplBlock { concept, ty, body, .. } = ast {
            self.impls.insert((concept, ty), body);
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Vec<AstNode>> {
        self.impls.get(&(concept.to_string(), ty.to_string())).cloned()
    }
}
