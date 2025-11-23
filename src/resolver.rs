// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use crate::specialization::{
    MonoKey, MonoValue, is_cache_safe, lookup_specialization, record_specialization,
};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I64,
    F32,
    Bool,
    Named(String),
    Unknown,
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

        // Fast-path: i64 implements Addable
        let mut addable = HashMap::new();
        addable.insert("add".to_string(), (vec![Type::I64], Type::I64));
        r.direct_impls
            .insert(("Addable".to_string(), Type::I64), addable);

        r
    }

    pub fn register(&mut self, ast: AstNode) {
        if let AstNode::ImplBlock { concept, ty, body } = ast {
            let ty = self.parse_type(&ty);
            let mut methods = HashMap::new();
            for node in body {
                if let AstNode::Method { name, params, ret } = node {
                    let sig = (
                        params
                            .into_iter()
                            .map(|(_, t)| self.parse_type(&t))
                            .collect(),
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
            AstNode::Call {
                receiver,
                method,
                type_args,
                args,
                ..
            } => {
                let recv_ty = receiver.as_ref().map(|r| self.infer_type(r));

                // Fast-path trait lookup
                if let Some(recv_ty) = recv_ty {
                    if let Some(impls) = self
                        .direct_impls
                        .get(&("Addable".to_string(), recv_ty.clone()))
                    {
                        if let Some((params, ret)) = impls.get(method) {
                            if params.len() == args.len() {
                                return ret.clone();
                            }
                        }
                    }
                }

                // Thin monomorphization + specialization cache
                let key = MonoKey {
                    func_name: method.clone(),
                    type_args: type_args.clone(),
                };

                if let Some(cached) = lookup_specialization(&key) {
                    Type::Named(cached.llvm_func_name)
                } else {
                    let mut mangled = method.clone();
                    if !type_args.is_empty() {
                        mangled.push_str("__");
                        for t in type_args {
                            mangled.push_str(&t.replace(['<', '>', ':'], "_"));
                            mangled.push('_');
                        }
                    }
                    let cache_safe = type_args.iter().all(|t| is_cache_safe(t));
                    record_specialization(
                        key,
                        MonoValue {
                            llvm_func_name: mangled.clone(),
                            cache_safe,
                        },
                    );
                    Type::Named(mangled)
                }
            }
            _ => Type::Unknown,
        }
    }

    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;
        for ast in asts {
            if let AstNode::FuncDef { body, .. } = ast {
                for stmt in body {
                    self.infer_type(stmt);
                    if !self.borrow_checker.check(stmt) {
                        ok = false;
                    }
                }
            }
        }
        ok
    }

    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = MirGen::new();
        mir_gen.gen_mir(ast)
    }

    pub fn fold_semiring_chains(&self, mir: &mut Mir) -> bool {
        let changed = false;
        let mut i = 0;
        while i + 1 < mir.stmts.len() {
            if let (
                MirStmt::Call {
                    func: f1,
                    args: a1,
                    dest: d1,
                },
                MirStmt::Call {
                    func: f2, args: a2, ..
                },
            ) = (&mir.stmts[i], &mir.stmts[i + 1])
            {
                if f1 == "add" && f2 == "add" && a2[0] == *d1 {
                    mir.stmts[i] = MirStmt::SemiringFold {
                        op: SemiringOp::Add,
                        values: vec![a1[0], a1[1], a2[1]],
                        result: a2[1], // reuse last dest
                    };
                    mir.stmts.remove(i + 1);
                    return true;
                }
            }
            i += 1;
        }
        changed
    }
}
