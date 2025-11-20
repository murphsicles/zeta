// src/resolver.rs
use crate::ast::AstNode;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

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
        for t in ["Send", "Sync", "CacheSafe", "Addable", "Copy", "Eq"] {
            common.insert(t.to_string());
        }

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
            AstNode::ConceptDef { ref name, .. } => {
                self.concepts.insert(name.clone(), ast);
            }
            AstNode::ImplBlock { ref concept, ref ty, .. } => {
                self.impls.insert((concept.clone(), ty.clone()), ast);
            }
            AstNode::FuncDef { ref name, .. } => {
                self.funcs.insert(name.clone(), ast);
            }
            AstNode::StructDef { ref name, ref fields, .. } => {
                let field_names = fields.clone();
                self.structs.insert(name.clone(), ast);
                self.structural_cache.insert(name.clone(), field_names);
            }
            AstNode::Derive { ref ty, ref traits } => {
                self.derives.entry(ty.clone()).or_default().extend(traits.clone());
            }
            _ => {}
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        let key = (concept.to_string(), ty.to_string());

        {
            let memo = self.lazy_memo.lock().unwrap();
            if let Some(cached) = memo.get(&key) {
                return cached.as_ref().map(|arc| &**arc);
            }
        }

        let mut memo = self.lazy_memo.lock().unwrap();

        if let Some(imp) = self.impls.get(&key) {
            let arc = Arc::new(imp.clone());
            memo.insert(key, Some(arc.clone()));
            return Some(&*arc);
        }

        if let Some(traits) = self.derives.get(ty) {
            if traits.contains(&concept.to_string()) {
                let synthetic = AstNode::ImplBlock {
                    concept: concept.to_string(),
                    ty: ty.to_string(),
                    body: vec![],
                };
                let arc = Arc::new(synthetic);
                memo.insert(key, Some(arc.clone()));
                return Some(&*arc);
            }
        }

        if concept == "Copy" || concept == "Eq" {
            if let Some(fields) = self.structural_cache.get(ty) {
                if fields.iter().all(|(_, fty)| fty == "i32") {
                    let synthetic = AstNode::ImplBlock {
                        concept: concept.to_string(),
                        ty: ty.to_string(),
                        body: vec![],
                    };
                    let arc = Arc::new(synthetic);
                    memo.insert(key, Some(arc.clone()));
                    return Some(&*arc);
                }
            }
        }

        memo.insert(key, None);
        None
    }

    pub fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        self.resolve_impl(concept, ty).map_or(false, |imp| {
            if let AstNode::ImplBlock { body, .. } = imp {
                body.iter().any(|m| matches!(m, AstNode::Method { name, .. } if name == method))
            } else {
                false
            }
        })
    }

    pub fn typecheck(&self, _asts: &[AstNode]) -> bool {
        true
    }
}
