// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use crate::mir::{Mir, MirExpr, MirGen, MirStmt, SemiringOp};
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MonoKey {
    pub func: String,
    pub types: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitKind {
    Nominal(String),                   // Explicit impl
    Structural(Vec<(String, String)>), // Fields/methods match
    AutoDerived(String),               // Regularity-based derive
}

#[derive(Debug)]
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
    structural_cache: HashMap<(String, Vec<(String, String)>), bool>,
    regularity_cache: HashMap<String, Vec<String>>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut res = Self {
            concepts: HashMap::new(),
            impls: HashMap::new(),
            funcs: HashMap::new(),
            common_traits: vec![
                "Send".to_string(),
                "Sync".to_string(),
                "Addable".to_string(),
                "CacheSafe".to_string(),
                "Copy".to_string(),
                "Eq".to_string(),
            ],
            mir_cache: HashMap::new(),
            lazy_memo: Mutex::new(HashMap::new()),
            ctfe_cache: HashMap::new(),
            mono_cache: HashMap::new(),
            mono_mir: HashMap::new(),
            structural_cache: HashMap::new(),
            regularity_cache: HashMap::new(),
        };
        res.common_traits.sort();
        res
    }

    pub fn register(&mut self, ast: AstNode) {
        let ast_hash = format!("{:?}", ast);
        match &ast {
            AstNode::ConceptDef { name, .. } => {
                self.concepts.insert(name.clone(), ast.clone());
            }
            AstNode::ImplBlock { concept, ty, .. } => {
                self.impls
                    .insert((concept.clone(), ty.clone()), ast.clone());
            }
            AstNode::FuncDef { name, .. } => {
                self.funcs.insert(name.clone(), ast.clone());
            }
            AstNode::Derive { ty, traits } => {
                for tr in traits {
                    let auto_traits = self.auto_derive_traits(ty);
                    for auto_tr in auto_traits {
                        self.impls.insert(
                            (auto_tr.clone(), ty.clone()),
                            AstNode::ImplBlock {
                                concept: auto_tr,
                                ty: ty.clone(),
                                body: vec![],
                            },
                        );
                    }
                    self.impls.insert(
                        (tr.clone(), ty.clone()),
                        AstNode::ImplBlock {
                            concept: tr.clone(),
                            ty: ty.clone(),
                            body: vec![],
                        },
                    );
                }
            }
            AstNode::StructDef { name, fields, .. } => {
                let field_map: Vec<(String, String)> = fields
                    .iter()
                    .map(|(fname, fty)| (fname.clone(), fty.clone()))
                    .collect();
                self.structural_cache
                    .insert((name.clone(), field_map.clone()), true);
                self.compute_regularity(name, &field_map);
            }
            _ => {}
        }
        if let AstNode::FuncDef { generics, .. } = &ast {
            if generics.is_empty() {
                let mut mir_gen = MirGen::new();
                let mut mir = mir_gen.gen_mir(&ast);
                self.ctfe_eval(&mut mir);
                self.mir_cache.insert(ast_hash, mir);
            }
        }
    }

    fn compute_regularity(&mut self, ty: &str, fields: &Vec<(String, String)>) {
        let mut derivable = vec![];
        if fields
            .iter()
            .all(|(_, fty)| self.resolve_impl("Copy", fty).is_some() && fty == "i32")
        {
            derivable.push("Copy".to_string());
        }
        if fields
            .iter()
            .all(|(_, fty)| self.resolve_impl("Eq", fty).is_some())
        {
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
        orig_ast.clone()
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        let key = (concept.to_string(), ty.to_string());
        {
            let memo = self.lazy_memo.lock().unwrap();
            if let Some(entry) = memo.get(&key) {
                return entry.as_ref().map(|arc| &**arc);
            }
        }

        let mut memo = self.lazy_memo.lock().unwrap();

        if let Some(impl_ast) = self.impls.get(&key) {
            let arc_impl = Some(Arc::new(impl_ast.clone()));
            memo.insert(key.clone(), arc_impl.clone());
            return arc_impl.as_ref().map(|arc| &**arc);
        }

        let candidates: Vec<_> = self
            .impls
            .iter()
            .filter(|((c, t), _)| c == concept && t == ty)
            .collect();

        let impl_ast = candidates.first().map(|(_, a)| a.clone());
        let arc_impl = impl_ast.map(Arc::new);
        memo.insert(key, arc_impl.clone());
        arc_impl.as_ref().map(|arc| &**arc)
    }

    fn is_structural_match(&self, concept: &str, _ty: &str) -> bool {
        if concept == "Copy" {
            true
        } else {
            false
        }
    }

    pub fn infer_phantom(&self, ty: &str) -> String {
        if ty.contains("<") && !ty.contains(">") {
            format!("{}<()>", ty)
        } else {
            ty.to_string()
        }
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
                    self.resolve_impl("CacheSafe", &inferred_name).is_some() &&
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
                    self.concepts.contains_key(&inf_ty) && self.resolve_impl("CacheSafe", &inf_ty).is_some()
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
                        let borrow_ok = !body.iter().any(|node| !bc.check(node)) && bc.validate_affine(body) && bc.validate_speculative(body);
                        borrow_ok
                    }
                }
                _ => true,
            }
        })
    }

    pub fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        let inf_ty = self.infer_phantom(ty);
        self.resolve_impl(concept, &inf_ty)
            .map_or(false, |impl_ast| {
                if let AstNode::ImplBlock { body, .. } = impl_ast {
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

    fn ctfe_eval(&mut self, mir: &mut Mir) {
        let len = mir.stmts.len();
        for i in 0..len {
            if let MirStmt::Call { func, args } = &mir.stmts[i] {
                if func.contains("add") {
                    if let (Some(&recv), Some(&arg)) = (args.get(0), args.get(1)) {
                        let recv_lit = self.expr_to_lit(&mir, recv);
                        let arg_lit = self.expr_to_lit(&mir, arg);
                        if let (Some(MirExpr::Lit(a)), Some(MirExpr::Lit(b))) = (recv_lit, arg_lit)
                        {
                            let res = a + b;
                            let res_id = self.fresh_local(mir);
                            mir.stmts[i] = MirStmt::Assign {
                                lhs: res_id,
                                rhs: MirExpr::ConstEval(res),
                            };
                        }
                    }
                }
            }
        }
    }

    fn fresh_local(&self, _mir: &mut Mir) -> u32 {
        0
    }

    fn expr_to_lit(&self, _mir: &Mir, _id: u32) -> Option<MirExpr> {
        Some(MirExpr::Lit(0))
    }
}
