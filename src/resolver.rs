// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use crate::mir::{Mir, MirGen};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoKey(pub String, pub Vec<String>);

pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
    structs: HashMap<String, AstNode>,
    common_traits: HashSet<String>,
    memo_impl: Mutex<HashMap<(String, String), Option<Arc<AstNode>>>>,
    mir_cache: Mutex<HashMap<String, Mir>>,
    mono_cache: Mutex<HashMap<MonoKey, AstNode>>,
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
            memo_impl: Mutex::new(HashMap::new()),
            mir_cache: Mutex::new(HashMap::new()),
            mono_cache: Mutex::new(HashMap::new()),
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { ref name, .. } => {
                self.concepts.insert(name.clone(), ast);
            }
            AstNode::ImplBlock { ref concept, ref ty, .. } => {
                self.impls.insert((concept.clone(), ty.clone()), ast);
            }
            AstNode::FuncDef { ref name, .. } => {
                self.funcs.insert(name.clone(), ast);
            }
            AstNode::StructDef { ref name, .. } => {
                self.structs.insert(name.clone(), ast);
            }
            AstNode::Derive { ty, traits } => {
                for tr in traits {
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

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Arc<AstNode>> {
        let key = (concept.to_string(), ty.to_string());
        {
            let memo = self.memo_impl.lock().unwrap();
            if let Some(cached) = memo.get(&key) {
                return cached.clone();
            }
        }
        let result = self.impls.get(&key).map(|n| Arc::new(n.clone()));
        self.memo_impl.lock().unwrap().insert(key, result.clone());
        result
    }

    pub fn get_cached_mir(&self, key: &str) -> Option<Mir> {
        let cache = self.mir_cache.lock().unwrap();
        cache.get(key).cloned()
    }

    pub fn cache_mir(&self, key: String, mir: Mir) {
        let mut cache = self.mir_cache.lock().unwrap();
        cache.insert(key, mir);
    }

    pub fn monomorphize(&self, key: MonoKey, ast: &AstNode) -> AstNode {
        let mut cache = self.mono_cache.lock().unwrap();
        if let Some(cached) = cache.get(&key) {
            return cached.clone();
        }
        let mono = ast.clone(); // placeholder - real monomorphization later
        cache.insert(key.clone(), mono.clone());
        mono
    }

    pub fn get_mono_mir(&self, key: &MonoKey) -> Option<Mir> {
        let hash = format!("{:?}", key);
        self.get_cached_mir(&hash)
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        asts.iter().all(|ast| {
            match ast {
                AstNode::FuncDef { params, body, attrs, .. } => {
                    let stable_abi_ok = !attrs.contains(&"stable_abi".to_string())
                        || !params.iter().any(|(_, t)| t.contains('<'));
                    let mut bc = BorrowChecker::new();
                    for (n, _) in params {
                        bc.declare(n.clone(), BorrowState::Owned);
                    }
                    let borrow_ok = body.iter().all(|n| bc.check(n))
                        && bc.validate_affine(body)
                        && bc.validate_speculative(body);
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
}
