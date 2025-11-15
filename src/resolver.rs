// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use crate::mir::{MirGen, Mir};
use std::collections::{HashMap, Mutex};
use std::sync::Arc;
use rayon::prelude::*; // Parallel Chalk++ stub

#[derive(Debug, Clone)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
    common_traits: Vec<String>,
    // Incremental MIR cache: Key = Ast hash, Val = Mir
    mir_cache: HashMap<String, Mir>,
    // Lazy res: Memoized impl lookups
    lazy_memo: Mutex<HashMap<(String, String), Option<Arc<AstNode>>>>,
}

impl Resolver {
    pub fn new() -> Self { 
        let mut res = Self { 
            concepts: HashMap::new(), impls: HashMap::new(), funcs: HashMap::new(), 
            common_traits: vec!["Send".to_string(), "Sync".to_string(), "Addable".to_string()], 
            mir_cache: HashMap::new(), lazy_memo: Mutex::new(HashMap::new()) 
        };
        res.common_traits.sort();
        res
    }

    pub fn register(&mut self, ast: AstNode) {
        let ast_hash = format!("{:?}", ast); // Stub hash
        match &ast {
            AstNode::ConceptDef { name, .. } => { self.concepts.insert(name.clone(), ast); }
            AstNode::ImplBlock { concept, ty, .. } => { self.impls.insert((concept.clone(), ty.clone()), ast); }
            AstNode::FuncDef { name, .. } => { self.funcs.insert(name.clone(), ast); }
            AstNode::Derive { ty, traits } => {
                for tr in traits {
                    self.impls.insert((tr.clone(), ty.clone()), AstNode::ImplBlock { concept: tr.clone(), ty: ty.clone(), body: vec![] });
                }
            }
            _ => {}
        }
        // Cache MIR if FuncDef
        if matches!(&ast, AstNode::FuncDef { .. }) {
            let mut gen = MirGen::new();
            let mir = gen.gen_mir(&ast);
            self.mir_cache.insert(ast_hash, mir);
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        // Lazy memoized lookup
        let key = (concept.to_string(), ty.to_string());
        let memo = self.lazy_memo.lock().unwrap();
        if let Some(cached) = memo.get(&key) {
            return cached.as_ref().and_then(|arc| (**arc).as_ref());
        }
        drop(memo);

        // Parallel Chalk++: Parallel search impls (stub: rayon over impls keys)
        let candidates: Vec<_> = self.impls.par_iter()
            .filter(|((c, t), _)| c == concept && t == ty)
            .collect();
        let impl_ast = if candidates.is_empty() {
            // Auto-derive/infer for common
            if self.common_traits.contains(&concept.to_string()) {
                if concept == "Copy" && (ty == "i32" || ty.contains("Range")) {
                    return Some(&AstNode::ImplBlock { concept: concept.to_string(), ty: ty.to_string(), body: vec![] });
                }
            }
            None
        } else {
            candidates.first().map(|(_, a)| a)
        };

        // Cache result
        let mut memo = self.lazy_memo.lock().unwrap();
        let arc_impl = impl_ast.map(|i| Arc::new(i.clone()));
        memo.insert(key, arc_impl.clone());
        arc_impl.and_then(|arc| (**arc).as_ref())
    }

    pub fn infer_phantom(&self, ty: &str) -> String {
        if ty.contains("<") && !ty.contains(">") { format!("{}<()>", ty) } else { ty.to_string() }
    }

    pub fn get_cached_mir(&self, ast_hash: &str) -> Option<&Mir> {
        self.mir_cache.get(ast_hash)
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        asts.par_iter().all(|ast| { // Parallel typecheck
            match ast {
                AstNode::ActorDef { name, methods } => { 
                    let inferred_name = self.infer_phantom(name);
                    if self.resolve_impl("Send", &inferred_name).is_none() || self.resolve_impl("Sync", &inferred_name).is_none() { return false; }
                    methods.iter().all(|method| {
                        if let AstNode::Method { params, .. } = method {
                            params.iter().all(|(_, pty)| {
                                let inf_pt = self.infer_phantom(pty);
                                self.resolve_impl("Send", &inf_pt).is_some()
                            })
                        } else { true }
                    })
                }
                AstNode::SpawnActor { actor_ty, .. } => {
                    let inf_ty = self.infer_phantom(actor_ty);
                    self.concepts.contains_key(&inf_ty) && self.resolve_impl("Send", &inf_ty).is_some()
                }
                AstNode::TimingOwned { ty, .. } => {
                    let inf_ty = self.infer_phantom(ty);
                    self.concepts.contains_key(&inf_ty)
                }
                AstNode::FuncDef { where_clause, body, params, attrs, ret, generics, .. } => {
                    for g in generics { if g.ends_with("=Self") { /* stub */ } }
                    if attrs.contains(&"stable_abi".to_string()) {
                        params.iter().any(|(_, pt)| pt.contains("<")) || ret.contains("<")
                    } else { false }
                    if let Some(bounds) = where_clause {
                        bounds.iter().any(|(ty, concept)| {
                            let inf_ty = self.infer_phantom(ty);
                            self.resolve_impl(concept, &inf_ty).is_none()
                        })
                    } else { false }
                    if attrs.contains(&"ai_opt".to_string()) {
                        !body.iter().any(|n| matches!(n, AstNode::Call { .. }))
                    } else { false }
                    let mut bc = BorrowChecker::new();
                    for (pname, _) in params {
                        bc.declare(pname.clone(), BorrowState::Owned);
                    }
                    !body.iter().any(|node| !bc.check(node))
                }
                _ => true,
            }
        })
    }

    pub fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        let inf_ty = self.infer_phantom(ty);
        self.resolve_impl(concept, &inf_ty).map_or(false, |impl_ast| {
            if let AstNode::ImplBlock { body, .. } = impl_ast { 
                body.iter().any(|m| if let AstNode::Method { name, .. } = m { name == method } else { false }) 
            } else { false }
        })
    }
}
