// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
    // Auto-import common traits
    common_traits: Vec<String>,
}

impl Resolver {
    pub fn new() -> Self { 
        let mut res = Self { concepts: HashMap::new(), impls: HashMap::new(), funcs: HashMap::new(), common_traits: vec!["Send".to_string(), "Sync".to_string(), "Addable".to_string()] };
        res.common_traits.sort();
        res
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { name, .. } => { self.concepts.insert(name, ast); }
            AstNode::ImplBlock { concept, ty, .. } => { self.impls.insert((concept, ty), ast); }
            AstNode::FuncDef { name, .. } => { self.funcs.insert(name, ast); }
            AstNode::Derive { ty, traits } => {
                // Auto-gen impls for derives
                for tr in traits {
                    self.impls.insert((tr, ty.clone()), AstNode::ImplBlock { concept: tr, ty, body: vec![] }); // Stub body
                }
            }
            _ => {}
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        // Auto-import: Check common first
        if self.common_traits.contains(&concept.to_string()) && !self.impls.contains_key(&(concept.to_string(), ty.to_string())) {
            // Infer/derive if missing (e.g., for Copy on Range<T>, assume yes for primitives)
            if concept == "Copy" && (ty == "i32" || ty.contains("Range")) { return Some(&AstNode::ImplBlock { concept: concept.to_string(), ty: ty.to_string(), body: vec![] }); }
        }
        self.impls.get(&(concept.to_string(), ty.to_string()))
    }

    // Phantom infer: Stub, assume infer from context (e.g., unused gen param = ())
    pub fn infer_phantom(&self, ty: &str) -> String {
        if ty.contains("<") && !ty.contains(">") { format!("{}<()>", ty) } else { ty.to_string() }
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        for ast in asts {
            match ast {
                AstNode::ActorDef { name, methods } => { 
                    let inferred_name = self.infer_phantom(name);
                    if self.resolve_impl("Send", &inferred_name).is_none() || self.resolve_impl("Sync", &inferred_name).is_none() { return false; }
                    for method in methods {
                        if let AstNode::Method { params, .. } = method {
                            for (_, pty) in params {
                                let inf_pt = self.infer_phantom(pty);
                                if self.resolve_impl("Send", &inf_pt).is_none() { return false; }
                            }
                        }
                    }
                }
                AstNode::SpawnActor { actor_ty, .. } => {
                    let inf_ty = self.infer_phantom(actor_ty);
                    if !self.concepts.contains_key(&inf_ty) || self.resolve_impl("Send", &inf_ty).is_none() { return false; }
                }
                AstNode::TimingOwned { ty, .. } => {
                    let inf_ty = self.infer_phantom(ty);
                    if !self.concepts.contains_key(&inf_ty) { return false; }
                }
                AstNode::FuncDef { where_clause, body, params, attrs, ret, generics, .. } => {
                    // Const defaults: Infer from generics
                    for g in generics { if g.ends_with("=Self") { /* stub infer */ } }
                    if attrs.contains(&"stable_abi".to_string()) {
                        for (_, pt) in params.iter() { if pt.contains("<") { return false; } }
                        if ret.contains("<") { return false; }
                    }
                    if let Some(bounds) = where_clause {
                        for (ty, concept) in bounds {
                            let inf_ty = self.infer_phantom(ty);
                            if self.resolve_impl(concept, &inf_ty).is_none() { return false; }
                        }
                    }
                    if attrs.contains(&"ai_opt".to_string()) {
                        if !body.iter().any(|n| matches!(n, AstNode::Call { .. })) { return false; }
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
        let inf_ty = self.infer_phantom(ty);
        self.resolve_impl(concept, &inf_ty).map_or(false, |impl_ast| {
            if let AstNode::ImplBlock { body, .. } = impl_ast { body.iter().any(|m| if let AstNode::Method { name, .. } = m { name == method } else { false }) } else { false }
        })
    }
}
