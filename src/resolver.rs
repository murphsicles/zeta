// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use rayon  std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoKey {
    pub func: String,
    pub types: Vec<String>,
}

#[derive(Debug)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
    structs: HashMap<String, AstNode>,
    derives: HashMap<String, Vec<String>>,
    common_traits: HashSet<String>,
    lazy_memo: Mutex<HashMap<(String, String), Option<Arc<AstNode>>>>,
    structural_cache: HashMap<String, Vec<(String, String)>>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut common = HashSet::new();
        common.insert("Send".to_string());
        common.insert("Sync".to_string());
        common.insert("CacheSafe".to_string());
        common.insert("Addable".to_string());
        common.insert("Copy".to_string());
        common.insert("Eq".to_string());

        Self {
            concepts: HashMap::new(),
            impls: HashMap::new(),
            funcs: HashMap::new(),
            structs: HashMap::new(),
            derives: HashMap::new(),
            common_traits: common,
            lazy_memo: Mutex::new(HashMap::new()),
            structural_cache: HashMap::new(),
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
                let field_names: Vec<(String, String)> = fields.clone();
                self.structs.insert(name.clone(), ast);
                self.structural_cache.insert(name, field_names);
            }
            AstNode::Derive { ty, traits } => {
                self.derives.entry(ty).or_default().extend(traits);
            }
            _ => {}
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        let key = (concept.to_string(), ty.to_string());

        // Check memo first
        {
            let memo = self.lazy_memo.lock().unwrap();
            if let Some(cached) = memo.get(&key) {
                return cached.as_ref().map(|arc| &**arc);
            }
        }

        let mut memo = self.lazy_memo.lock().unwrap();

        // Direct impl
        if let Some(imp) = self.impls.get(&key) {
            let arc = Arc::new(imp.clone());
            memo.insert(key, Some(arc.clone()));
            return Some(&arc);
        }

        // Derive-generated impl
        if let Some(traits) = self.derives.get(ty) {
            if traits.contains(&concept.to_string()) {
                let synthetic = AstNode::ImplBlock {
                    concept: concept.to_string(),
                    ty: ty.to_string(),
                    body: vec![],
                };
                let arc = Arc::new(synthetic);
                memo.insert(key, Some(arc.clone()));
                return Some(&arc);
            }
        }

        // Structural fallback for Copy/Eq on primitive-field structs
        if concept == "Copy" || concept == "Eq" {
            if let Some(fields) = self.structural_cache.get(ty) {
                let all_primitive = fields.iter().all(|(_, fty)| fty == "i32");
                if all_primitive {
                    let synthetic = AstNode::ImplBlock {
                        concept: concept.to_string(),
                        ty: ty.to_string(),
                        body: vec![],
                    };
                    let arc = Arc::new(synthetic);
                    memo.insert(key, Some(arc.clone()));
                    return Some(&arc);
                }
            }
        }

        memo.insert(key, None);
        None
    }

    pub fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        self.resolve_impl(concept, ty).map_or(false, |imp| {
            if let AstNode::ImplBlock { body, .. } = imp {
                body.iter().any(|m| {
                    if let AstNode::Method { name, .. } = m {
                        name == method
                    } else {
                        false
                    }
                })
            } else {
                false
            }
        })
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        let mut ok = true;

        for ast in asts {
            match ast {
                AstNode::FuncDef { body, params, attrs, .. } => {
                    // stable_abi restriction
                    if attrs.contains(&"stable_abi".to_string()) {
                        if params.iter().any(|(_, t)| t.contains("<")) {
                            ok = false;
                        }
                    }

                    // Borrow checking
                    let mut bc = BorrowChecker::new();
                    for (name, _) in params {
                        bc.declare(name.clone(), BorrowState::Owned);
                    }
                    for stmt in body {
                        if !bc.check(stmt) {
                            ok = false;
                        }
                    }
                    if !(bc.validate_affine(body) && bc.validate_speculative(body)) {
                        ok = false;
                    }
                }
                AstNode::Call { receiver, method, .. } => {
                    let receiver_ty = match receiver.as_ref() {
                        AstNode::Var(v) => v.as_str(),
                        _ => "unknown",
                    };
                    if method == "add" && !self.has_method("Addable", receiver_ty, "add") {
                        ok = false;
                    }
                }
                _ => {}
            }
        }
        ok
    }
}
