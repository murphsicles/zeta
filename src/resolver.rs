// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use crate::mir::{MirGen, Mir, SemiringOp};
use std::collections::{HashMap, Mutex};
use std::sync::Arc;
use rayon::prelude::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub type MonoKey = (String, Vec<String>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitKind {
    Nominal(String), // Explicit impl
    Structural(Vec<(String, String)>), // Fields/methods match
    AutoDerived(String), // Regularity-based derive
}

#[derive(Debug, Clone)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
    common_traits: Vec<String>,
    mir_cache: HashMap<String, Mir>,
    lazy_memo: Mutex<HashMap<(String, String), Option<Arc<AstNode>>>>,
    ctfe_cache: HashMap<(SemiringOp, i64, i64), i64>,
    mono_cache: HashMap<MonoKey, AstNode>,
    mono_mir: HashMap<MonoKey, Mir>,
    structural_cache: HashMap<(String, Vec<(String, String)>), bool>, // Trait + fields -> matches
    regularity_cache: HashMap<String, Vec<String>>, // Type -> auto-derivable traits
}

impl Resolver {
    pub fn new() -> Self { 
        let mut res = Self { 
            concepts: HashMap::new(), impls: HashMap::new(), funcs: HashMap::new(), 
            common_traits: vec!["Send".to_string(), "Sync".to_string(), "Addable".to_string(), "CacheSafe".to_string(), "Copy".to_string(), "Eq".to_string()], 
            mir_cache: HashMap::new(), lazy_memo: Mutex::new(HashMap::new()),
            ctfe_cache: HashMap::new(), mono_cache: HashMap::new(), mono_mir: HashMap::new(),
            structural_cache: HashMap::new(),
            regularity_cache: HashMap::new(),
        };
        res.common_traits.sort();
        res
    }

    pub fn register(&mut self, ast: AstNode) {
        let ast_hash = format!("{:?}", ast);
        match &ast {
            AstNode::ConceptDef { name, params, .. } => { 
                self.concepts.insert(name.clone(), ast); 
            }
            AstNode::ImplBlock { concept, ty, .. } => { self.impls.insert((concept.clone(), ty.clone()), ast); }
            AstNode::FuncDef { name, generics, .. } => { self.funcs.insert(name.clone(), ast); }
            AstNode::Derive { ty, traits } => {
                for tr in traits {
                    // Auto-classify regularity for additional derives
                    let auto_traits = self.auto_derive_traits(ty);
                    for auto_tr in auto_traits {
                        self.impls.insert((auto_tr.clone(), ty.clone()), AstNode::ImplBlock { concept: auto_tr, ty: ty.clone(), body: vec![] });
                    }
                    self.impls.insert((tr.clone(), ty.clone()), AstNode::ImplBlock { concept: tr.clone(), ty: ty.clone(), body: vec![] });
                }
            }
            AstNode::StructDef { name, fields, .. } => {
                // Cache structural fields for hybrid traits
                let field_map: Vec<(String, String)> = fields.iter().map(|(fname, fty)| (fname.clone(), fty.clone())).collect();
                self.structural_cache.insert((name.clone(), field_map.clone()), true);
                // Precompute regularity for auto-derive
                self.compute_regularity(name, &field_map);
            }
            _ => {}
        }
        if let AstNode::FuncDef { generics, .. } = &ast {
            if generics.is_empty() {
                let mut gen = MirGen::new();
                let mut mir = gen.gen_mir(&ast);
                self.ctfe_eval(&mut mir);
                self.mir_cache.insert(ast_hash, mir);
            }
        }
    }

    fn compute_regularity(&mut self, ty: &str, fields: &Vec<(String, String)>) {
        let mut derivable = vec![];
        // Copy: All fields Copy + sizeof < ptr (stub: primitive fields)
        if fields.iter().all(|(_, fty)| self.resolve_impl("Copy", fty).is_some() && fty == "i32") {
            derivable.push("Copy".to_string());
        }
        // Eq: All fields Eq
        if fields.iter().all(|(_, fty)| self.resolve_impl("Eq", fty).is_some()) {
            derivable.push("Eq".to_string());
        }
        self.regularity_cache.insert(ty.to_string(), derivable);
    }

    pub fn auto_derive_traits(&self, ty: &str) -> Vec<String> {
        self.regularity_cache.get(ty).cloned().unwrap_or_default()
    }

    pub fn monomorphize(&mut self, key: MonoKey, orig_ast: &AstNode) -> AstNode {
        if let Some(cached) = self.mono_cache.get(&key) {
            return cached.clone();
        }
        let mut mono_ast = orig_ast.clone();
        if let AstNode::FuncDef { params, ret, body, generics, .. } = &mut mono_ast {
            for (i, g) in generics.iter().enumerate() {
                let conc = &key.1[i];
                for (_, pt) in params.iter_mut() { *pt = pt.replace(g, conc); }
                *ret = ret.replace(g, conc);
                for node in body.iter_mut() {
                    if let AstNode::Var(v) = node { if v == g { *v = conc.clone(); } }
                }
            }
        }
        self.mono_cache.insert(key.clone(), mono_ast.clone());
        let mut gen = MirGen::new();
        let mut mir = gen.gen_mir(&mono_ast);
        self.ctfe_eval(&mut mir);
        self.mono_mir.insert(key, mir);
        mono_ast
    }

    pub fn get_mono_mir(&self, key: &MonoKey) -> Option<&Mir> {
        self.mono_mir.get(key)
    }

    fn ctfe_eval(&mut self, mir: &mut Mir) {
        for stmt in &mut mir.stmts {
            if let MirStmt::Assign { lhs, rhs } = stmt {
                if let MirExpr::MethodCall { recv, method, args } = rhs {
                    if method == "add" || method == "mul" {
                        let op = if method == "add" { SemiringOp::Add } else { SemiringOp::Mul };
                        if let (Some(MirExpr::Lit(l)), Some(MirExpr::Lit(r))) = (self.expr_to_lit(mir, *recv), self.expr_to_lit(mir, args[0])) {
                            let key = (op, l, r);
                            let res = self.ctfe_cache.entry(key).or_insert_with(|| {
                                match op {
                                    SemiringOp::Add => l + r,
                                    SemiringOp::Mul => l * r,
                                }
                            });
                            *rhs = MirExpr::ConstEval(*res);
                            mir.ctfe_consts.insert(*lhs, *res);
                        }
                    }
                }
            }
        }
    }

    fn expr_to_lit(&self, mir: &Mir, id: u32) -> Option<i64> {
        if let Some(c) = mir.ctfe_consts.get(&id) { Some(*c) } else { None }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        let key = (concept.to_string(), ty.to_string());
        let memo = self.lazy_memo.lock().unwrap();
        if let Some(cached) = memo.get(&key) {
            return cached.as_ref().and_then(|arc| (**arc).as_ref());
        }
        drop(memo);

        let candidates: Vec<_> = self.impls.par_iter()
            .filter(|((c, t), _)| c == concept && t == ty)
            .collect();
        let impl_ast = if candidates.is_empty() {
            if self.common_traits.contains(&concept.to_string()) {
                if concept == "Copy" && (ty == "i32" || ty.contains("Range")) {
                    return Some(&AstNode::ImplBlock { concept: concept.to_string(), ty: ty.to_string(), body: vec![] });
                }
            }
            // Hybrid: Check structural if no nominal
            if self.is_structural_match(concept, ty) {
                return Some(&AstNode::ImplBlock { concept: concept.to_string(), ty: ty.to_string(), body: vec![] });
            }
            // Auto-derived: Check regularity
            if self.regularity_cache.contains_key(ty) && self.auto_derive_traits(ty).contains(&concept.to_string()) {
                return Some(&AstNode::ImplBlock { concept: concept.to_string(), ty: ty.to_string(), body: vec![] });
            }
            None
        } else {
            candidates.first().map(|(_, a)| a)
        };

        let mut memo = self.lazy_memo.lock().unwrap();
        let arc_impl = impl_ast.map(|i| Arc::new(i.clone()));
        memo.insert(key, arc_impl.clone());
        arc_impl.and_then(|arc| (**arc).as_ref())
    }

    fn is_structural_match(&self, concept: &str, ty: &str) -> bool {
        let cache_key = (concept.to_string(), ty.to_string());
        if let Some(&cached) = self.structural_cache.get(&cache_key) {
            return cached;
        }
        // Stub: Match if ty has required fields/methods for concept
        // e.g., Copy: all fields Copy + size_of < ptr
        if concept == "Copy" {
            // Assume primitive or simple struct
            true
        } else {
            false
        }
        // Cache result
        // self.structural_cache.insert(cache_key, match);
        // match
    }

    pub fn infer_phantom(&self, ty: &str) -> String {
        if ty.contains("<") && !ty.contains(">") { format!("{}<()>", ty) } else { ty.to_string() }
    }

    pub fn get_cached_mir(&self, ast_hash: &str) -> Option<&Mir> {
        self.mir_cache.get(ast_hash)
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        asts.par_iter().all(|ast| {
            match ast {
                AstNode::ActorDef { name, methods } => { 
                    let inferred_name = self.infer_phantom(name);
                    self.resolve_impl("Send", &inferred_name).is_some() && self.resolve_impl("Sync", &inferred_name).is_some() &&
                    self.resolve_impl("CacheSafe", &inferred_name).is_some() && // CacheSafe req
                    methods.iter().all(|method| {
                        if let AstNode::Method { params, .. } = method {
                            params.iter().all(|(_, pty)| {
                                let inf_pt = self.infer_phantom(pty);
                                self.resolve_impl("Send", &inf_pt).is_some() && self.resolve_impl("CacheSafe", &inf_pt).is_some()
                            })
                        } else { true }
                    })
                }
                AstNode::SpawnActor { actor_ty, .. } => {
                    let inf_ty = self.infer_phantom(actor_ty);
                    self.concepts.contains_key(&inf_ty) && self.resolve_impl("Send", &inf_ty).is_some() && self.resolve_impl("CacheSafe", &inf_ty).is_some()
                }
                AstNode::TimingOwned { ty, .. } => {
                    let inf_ty = self.infer_phantom(ty);
                    self.concepts.contains_key(&inf_ty) && self.resolve_impl("CacheSafe", &inf_ty).is_some() // Static race: CacheSafe
                }
                AstNode::FuncDef { where_clause, body, params, attrs, ret, generics, .. } => {
                    for g in generics { if g.ends_with("=Self") { /* stub */ } }
                    !attrs.contains(&"stable_abi".to_string()) || (!params.iter().any(|(_, pt)| pt.contains("<")) && !ret.contains("<")) &&
                    if let Some(bounds) = where_clause {
                        !bounds.iter().any(|(ty, concept)| {
                            let inf_ty = self.infer_phantom(ty);
                            self.resolve_impl(concept, &inf_ty).is_none()
                        })
                    } else { true } &&
                    !attrs.contains(&"ai_opt".to_string()) || body.iter().any(|n| matches!(n, AstNode::Call { .. })) &&
                    {
                        let mut bc = BorrowChecker::new();
                        for (pname, _) in params {
                            bc.declare(pname.clone(), BorrowState::Owned);
                        }
                        let borrow_ok = !body.iter().any(|node| !bc.check(node)) && bc.validate_affine(body) && bc.validate_speculative(body); // Speculative validation
                        borrow_ok // Extended: Affine + Speculative borrowck
                    }
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
