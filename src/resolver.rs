// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
}

impl Resolver {
    pub fn new() -> Self { Self { concepts: HashMap::new(), impls: HashMap::new(), funcs: HashMap::new() } }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { name, .. } => { self.concepts.insert(name, ast); }
            AstNode::ImplBlock { concept, ty, .. } => { self.impls.insert((concept, ty), ast); }
            AstNode::FuncDef { name, .. } => { self.funcs.insert(name, ast); }
            _ => {}
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        self.impls.get(&(concept.to_string(), ty.to_string()))
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        for ast in asts {
            match ast {
                AstNode::ActorDef { .. } => {} 
                AstNode::SpawnActor { actor_ty, .. } => {
                    if !self.concepts.contains_key(actor_ty) { return false; }
                }
                AstNode::TimingOwned { ty, .. } => {
                    // Track speculative exec in borrowck
                    if !self.concepts.contains_key(ty) { return false; }
                }
                AstNode::FuncDef { where_clause, body, params, attrs, ret, .. } => {
                    if attrs.contains(&"stable_abi".to_string()) {
                        for (pn, pt) in params.iter() { if pt.contains("<") { return false; } }
                        if ret.contains("<") { return false; }
                    }
                    if let Some(bounds) = where_clause {
                        for (ty, concept) in bounds { if self.resolve_impl(&concept, &ty).is_none() { return false; } }
                    }
                    let mut bc = BorrowChecker::new();
                    for (pname, _) in params {
                        bc.declare(pname.clone(), BorrowState::Owned);
                    }
                    for node in body {
                        if !bc.check(node) { return false; }
                    }
                }
                _ => {}
            }
        }
        true
    }

    pub fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        self.resolve_impl(concept, ty).map_or(false, |impl_ast| {
            if let AstNode::ImplBlock { body, .. } = impl_ast { body.iter().any(|m| if let AstNode::Method { name, .. } = m { name == method } else { false }) } else { false }
        })
    }
}
