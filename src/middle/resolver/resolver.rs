// src/middle/resolver/resolver.rs
//! # Resolver — Semantic Analysis & Monomorphization Engine
//!
//! Single source of truth for type resolution, specialization caching,
//! borrow checking coordination, and MIR lowering.

use crate::frontend::ast::AstNode;
use crate::frontend::borrow::BorrowChecker;
use crate::middle::mir::mir::Mir;
use crate::middle::specialization::{
    CACHE, MonoKey, MonoValue, is_cache_safe, record_specialization,
};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

pub type Type = String;

const SPECIALIZATION_CACHE_FILE: &str = ".zeta_specialization_cache.json";

#[derive(Serialize, Deserialize)]
struct CacheFile {
    entries: HashMap<MonoKey, MonoValue>,
}

pub struct Resolver {
    pub impls: HashMap<(String, String), Vec<AstNode>>,
    pub cached_mirs: HashMap<String, Mir>,
    pub mono_mirs: HashMap<MonoKey, Mir>,
    pub borrow_checker: RefCell<BorrowChecker>,
    pub associated_types: HashMap<(String, String), String>,
    pub ctfe_consts: HashMap<AstNode, i64>,
    funcs: HashMap<String, (Vec<(String, String)>, String, bool)>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut r = Self {
            impls: HashMap::new(),
            cached_mirs: HashMap::new(),
            mono_mirs: HashMap::new(),
            borrow_checker: RefCell::new(BorrowChecker::new()),
            associated_types: HashMap::new(),
            ctfe_consts: HashMap::new(),
            funcs: HashMap::new(),
        };
        r.load_specialization_cache();
        r
    }

    fn load_specialization_cache(&mut self) {
        let path = PathBuf::from(SPECIALIZATION_CACHE_FILE);
        if let Ok(data) = fs::read_to_string(&path) {
            if let Ok(cache) = serde_json::from_str::<CacheFile>(&data) {
                for (key, value) in cache.entries {
                    self.mono_mirs.insert(key.clone(), Mir::default());
                    record_specialization(key, value);
                }
            }
        }
    }

    pub fn persist_specialization_cache(&self) {
        let cache_guard = CACHE.read().unwrap();
        let entries = cache_guard.clone();
        let cache_file = CacheFile { entries };
        if let Ok(json) = serde_json::to_string_pretty(&cache_file) {
            let _ = fs::write(SPECIALIZATION_CACHE_FILE, json);
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::FuncDef {
                name, params, ret, ..
            } => {
                self.funcs.insert(name, (params, ret, false));
            }
            AstNode::ExternFunc { name, params, ret } => {
                self.funcs.insert(name, (params, ret, true));
            }
            AstNode::ImplBlock {
                concept, ty, body, ..
            } => {
                self.impls.insert((concept, ty), body.clone());
                for b in body {
                    self.register(b);
                }
            }
            AstNode::ConceptDef { methods, .. } => {
                for m in methods {
                    self.register(m);
                }
            }
            AstNode::Method {
                name, params, ret, ..
            } => {
                self.funcs.insert(name, (params, ret, false));
            }
            _ => {}
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Vec<AstNode>> {
        self.impls
            .get(&(concept.to_string(), ty.to_string()))
            .cloned()
    }

    pub fn is_abi_stable(&self, key: &MonoKey) -> bool {
        key.type_args.iter().all(|t| is_cache_safe(t))
    }

    pub fn record_mono(&mut self, key: MonoKey, mut mir: Mir) {
        let mangled = key.mangle();
        mir.name = Some(mangled.clone());
        self.mono_mirs.insert(key.clone(), mir);
        record_specialization(
            key.clone(),
            MonoValue {
                llvm_func_name: mangled,
                cache_safe: self.is_abi_stable(&key),
            },
        );
    }

    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = crate::middle::mir::r#gen::MirGen::new();
        mir_gen.lower_to_mir(ast)
    }

    pub fn collect_used_specializations(
        &mut self,
        asts: &[AstNode],
    ) -> HashMap<String, Vec<Vec<String>>> {
        let mut used = HashMap::new();
        fn walk(
            node: &AstNode,
            used: &mut HashMap<String, Vec<Vec<String>>>,
            resolver: &mut Resolver,
        ) {
            if let AstNode::Call {
                receiver,
                method,
                type_args,
                ..
            } = node
            {
                let mut args = type_args.clone();
                if let Some(r) = receiver {
                    let ty = resolver.infer_type(r);
                    if args.is_empty() {
                        args = vec![ty];
                    }
                }
                if !args.is_empty() {
                    used.entry(method.clone()).or_default().push(args);
                }
            }
            match node {
                AstNode::FuncDef { body, .. } => body.iter().for_each(|s| walk(s, used, resolver)),
                AstNode::Return(inner) => walk(inner, used, resolver),
                AstNode::ExprStmt { expr } => walk(expr, used, resolver),
                AstNode::BinaryOp { left, right, .. } => {
                    walk(left, used, resolver);
                    walk(right, used, resolver);
                }
                AstNode::If {
                    cond, then, else_, ..
                } => {
                    walk(cond, used, resolver);
                    then.iter().for_each(|s| walk(s, used, resolver));
                    else_.iter().for_each(|s| walk(s, used, resolver));
                }
                AstNode::Let { expr, .. } => walk(expr, used, resolver),
                AstNode::Assign(lhs, rhs) => {
                    walk(lhs, used, resolver);
                    walk(rhs, used, resolver);
                }
                AstNode::Call { receiver, args, .. } => {
                    if let Some(r) = receiver {
                        walk(r, used, resolver);
                    }
                    args.iter().for_each(|a| walk(a, used, resolver));
                }
                _ => {}
            }
        }
        for ast in asts {
            walk(ast, &mut used, self);
        }
        used
    }

    pub fn monomorphize(&self, key: MonoKey, ast: &AstNode) -> AstNode {
        let mut mono = ast.clone();
        let mut subst: HashMap<String, String> = key
            .type_args
            .iter()
            .cloned()
            .zip(key.type_args.iter().cloned())
            .collect();
        if !key.type_args.is_empty() {
            subst.insert("Self".to_string(), key.type_args[0].clone());
        }
        fn substitute(node: &mut AstNode, subst: &HashMap<String, String>) {
            match node {
                AstNode::FuncDef {
                    generics,
                    params,
                    ret,
                    body,
                    ..
                } => {
                    generics.clear();
                    for (_, ty) in params.iter_mut() {
                        if let Some(r) = subst.get(ty) {
                            *ty = r.clone();
                        }
                    }
                    if let Some(r) = subst.get(ret) {
                        *ret = r.clone();
                    }
                    for s in body {
                        substitute(s, subst);
                    }
                }
                AstNode::Call { type_args, .. } => type_args.clear(),
                AstNode::TimingOwned { ty, inner } => {
                    if let Some(r) = subst.get(ty) {
                        *ty = r.clone();
                    }
                    substitute(inner, subst);
                }
                AstNode::BinaryOp { left, right, .. } => {
                    substitute(left, subst);
                    substitute(right, subst);
                }
                AstNode::If {
                    cond, then, else_, ..
                } => {
                    substitute(cond, subst);
                    for s in then {
                        substitute(s, subst);
                    }
                    for s in else_ {
                        substitute(s, subst);
                    }
                }
                _ => {}
            }
        }
        substitute(&mut mono, &subst);
        mono
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Resolver {
    fn drop(&mut self) {
        self.persist_specialization_cache();
    }
}
