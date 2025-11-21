// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use crate::specialization::{lookup_specialization, record_specialization, MonoKey, MonoValue, is_cache_safe};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I64, F32, Bool, Named(String), Unknown,
}

#[derive(Clone)]
pub struct Resolver {
    direct_impls: HashMap<(String, Type), HashMap<String, (Vec<Type>, Type)>>,
    type_env: HashMap<String, Type>,
    borrow_checker: BorrowChecker,
}

impl Resolver {
    pub fn new() -> Self {
        let mut r = Self {
            direct_impls: HashMap::new(),
            type_env: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
        };
        // fast-path Addable<i64>
        let mut map = HashMap::new();
        map.insert("add".to_string(), (vec![Type::I64], Type::I64));
        r.direct_impls.insert(("Addable".to_string(), Type::I64), map);
        r
    }

    pub fn register(&mut self, ast: AstNode) {
        if let AstNode::ImplBlock { concept, ty, body } = ast {
            let ty = self.parse_type(&ty);
            let mut methods = HashMap::new();
            for node in body {
                if let AstNode::Method { name, params, ret } = node {
                    let sig = (
                        params.into_iter().map(|(_, t)| self.parse_type(&t)).collect(),
                        self.parse_type(&ret),
                    );
                    methods.insert(name, sig);
                }
            }
            self.direct_impls.insert((concept, ty), methods);
        }
    }

    fn parse_type(&self, s: &str) -> Type {
        match s {
            "i64" => Type::I64,
            "f32" => Type::F32,
            "bool" => Type::Bool,
            _ => Type::Named(s.to_string()),
        }
    }

    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_) => Type::I64,
            AstNode::Var(v) => self.type_env.get(v).cloned().unwrap_or(Type::Unknown),
            AstNode::Assign(name, expr) => {
                let ty = self.infer_type(expr);
                self.type_env.insert(name.clone(), ty.clone());
                ty
            }
            AstNode::Call { receiver, method, type_args, .. } => {
                let recv_ty = receiver.as_ref().map(|r| self.infer_type(r));
                let key = MonoKey {
                    func_name: method.clone(),
                    type_args: type_args.clone(),
                };
                if let Some(cached) = lookup_specialization(&key) {
                    Type::Named(cached.llvm_func_name)
                } else {
                    let concrete = format!("{method}__{}", type_args.join("_"));
                    let cache_safe = type_args.iter().all(|t| is_cache_safe(t));
                    record_specialization(key, MonoValue { llvm_func_name: concrete.clone(), cache_safe });
                    Type::Named(concrete)
                }
            }
            _ => Type::Unknown,
        }
    }

    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = MirGen::new();
        mir_gen.gen_mir(ast)
    }

    // typecheck / fold_semiring_chains unchanged (kept from previous working version)
}
