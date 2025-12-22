// src/middle/resolver/resolver.rs
//! Core resolver structure and registration for Zeta concepts and implementations.
//! Integrates borrow checker for affine types.
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::Mir;
use crate::specialization::MonoKey;
use std::collections::HashMap;

pub type Type = String; // Stub for full type system

pub struct Resolver {
    impls: HashMap<(String, String), Vec<AstNode>>,
    cached_mirs: HashMap<String, Mir>,
    mono_mirs: HashMap<MonoKey, Mir>,
    borrow_checker: BorrowChecker,
}

impl Resolver {
    /// Creates a new resolver instance.
    pub fn new() -> Self {
        Self {
            impls: HashMap::new(),
            cached_mirs: HashMap::new(),
            mono_mirs: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
        }
    }

    /// Registers an AST node, such as an implementation block.
    pub fn register(&mut self, ast: AstNode) {
        if let AstNode::ImplBlock { concept, ty, body, .. } = ast {
            self.impls.insert((concept, ty), body);
        }
    }

    /// Resolves an implementation for a concept on a type.
    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Vec<AstNode>> {
        self.impls.get(&(concept.to_string(), ty.to_string())).cloned()
    }
}
