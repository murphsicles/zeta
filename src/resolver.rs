// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use crate::mir::{Mir, MirGen, SemiringOp};
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoKey {
    pub func: String,
    pub types: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
    structs: HashMap<String, AstNode>,
    common_traits: HashSet<String>,
    mono_cache: HashMap<MonoKey, AstNode>,
    mir_cache: HashMap<String, Mir>,
    ctfe_cache: HashMap<(SemiringOp, i64, i64), i64>,
    structural_cache: HashMap<String, Vec<(String, String)>>,
    regularity_cache: HashMap<String, Vec<String>>,
    memo_impl: Mutex<HashMap<(String, String), Option<Arc<AstNode>>>>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut common_traits = HashSet::new();
        for t in ["Send", "Sync", "Addable", "CacheSafe", "Copy", "Eq"] {
            common_traits.insert(t.to_string());
        }
        Self {
            concepts: HashMap::new(),
            impls: HashMap::new(),
            funcs: HashMap::new(),
            structs: HashMap::new(),
            common_traits,
            mono_cache: HashMap::new(),
            mir_cache: HashMap::new(),
            ctfe_cache: HashMap::new(),
            structural_cache: HashMap::new(),
            regularity_cache: HashMap::new(),
            memo_impl: Mutex::new(HashMap::new()),
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { name, .. } => {
                self.concepts.insert(name, ast);
            }
            AstNode::ImplBlock { concept, ty, .. } => {
                self.impls.insert((concept, ty), ast);
            }
            AstNode::FuncDef { name, .. } => {
                self.funcs.insert(name, ast);
            }
            AstNode::StructDef { name, fields, .. } => {
                let field_vec = fields.clone();
                self.structs.insert(name.clone(), ast);
                self.structural_cache.insert(name.clone(), field_vec.clone());
                self.compute_regularity(&name, &field_vec);
            }
            AstNode::Derive { ty, traits } => {
                for tr in traits {
                    let auto = self.auto_derive_traits(&ty);
                    for a in &auto {
                        self.impls.insert((a.clone(), ty.clone()), AstNode::ImplBlock {
                            concept: a.clone(),
                            ty: ty.clone(),
                            body: vec![],
                        });
                    }
                    self.impls.insert((tr.clone(), ty.clone()), AstNode::ImplBlock {
                        concept: tr,
                        ty: ty.clone(),
                        body: vec![],
                    });
                }
            }
            _ => {}
        }
    }

    fn compute_regularity(&mut self, ty: &str, fields: &[(String, String)]) {
        let mut derivable = vec![];
        if fields.iter().all(|(_, fty)| fty == "i32") {
            derivable.push("Copy".to_string());
            derivable.push("Eq".to_string());
        }
        self.regularity_cache.insert(ty.to_string(), derivable);
    }

    pub fn auto_derive_traits(&self, ty: &str) -> Vec<String> {
        self.regularity_cache.get(ty).cloned().unwrap_or_default()
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Arc<AstNode>> {
        let key = (concept.to_string(), ty.to_string());
        {
            let memo = self.memo_impl.lock().unwrap();
            if let Some(cached) = memo.get(&key) {
                return cached.clone();
            }
        }
        let result = self.impls.get(&key).map(|node| Arc::new(node.clone()));
        {
            let mut memo = self.memo_impl.lock().unwrap();
            memo.insert(key, result.clone());
        }
        result
    }

    pub fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        if let Some(imp) = self.resolve_impl(concept, ty) {
            if let AstNode::ImplBlock { body, .. } = imp.as_ref() {
                return body.iter().any(|m| {
                    if let AstNode::Method { name, .. } = m {
                        name == method
                    } else {
                        false
                    }
                });
            }
        }
        false
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        asts.par_iter().all(|ast| {
            match ast {
                AstNode::FuncDef { params, body, attrs, ret, .. } => {
                    let stable_abi_ok = if attrs.contains(&"stable_abi".to_string()) {
                        !params.iter().any(|(_, p)| p.contains('<')) && !ret.contains('<')
                    } else {
                        true
                    };

                    let mut bc = BorrowChecker::new();
                    for (name, _) in params {
                        bc.declare(name.clone(), BorrowState::Owned);
                    }
                    let borrow_ok = body.iter().all(|n| bc.check(n)) && bc.validate_affine(body) && bc.validate_speculative(body);

                    stable_abi_ok && borrow_ok
                }
                AstNode::TimingOwned { ty, .. } => self.resolve_impl("CacheSafe", ty).is_some(),
                AstNode::SpawnActor { actor_ty, .. } => {
                    self.resolve_impl("Send", actor_ty).is_some()
                        && self.resolve_impl("CacheSafe", actor_ty).is_some()
                }
                _ => true,
            }
        })
    }

    pub fn monomorphize(&mut self, key: MonoKey, orig: &AstNode) -> AstNode {
        if let Some(cached) = self.mono_cache.get(&key) {
            return cached.clone();
        }
        let mono = orig.clone(); // stub - real impl later
        self.mono_cache.insert(key, mono.clone());
        mono
    }

    pub fn ctfe_eval(&mut self, mir: &mut Mir) {
        // Simple add folding
        for stmt in &mut mir.stmts {
            if let crate::mir::MirStmt::Call { func, args } = stmt {
                if func.contains("add") && args.len() == 2 {
                    // placeholder - real CTFE later
                }
            }
        }
    }
}
