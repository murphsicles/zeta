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

type MethodSig = (Vec<Type>, Type);
type ImplMethods = HashMap<String, MethodSig>;

#[derive(Clone)]
pub struct Resolver {
    direct_impls: HashMap<(String, Type), ImplMethods>,
    type_env: HashMap<String, Type>,
    borrow_checker: BorrowChecker,
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
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
                if let Some(recv_ty) = recv_ty
                    && let Some(impls) = self.direct_impls.get(&("Addable".to_string(), recv_ty.clone()))
                    && let Some((params, ret)) = impls.get(method)
                    && params.len() == args.len() {
                    return ret.clone();
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
        let mut changed = false;
        let mut i = 0;
        while i + 1 < mir.stmts.len() {
            if let MirStmt::Call { func: ref f1, args: ref a1, dest: ref d1 } = &mir.stmts[i] {
                if f1.as_str() != "add" {
                    i += 1;
                    continue;
                }
                if let MirStmt::Call { func: ref f2, args: ref a2, dest: ref d2 } = &mir.stmts[i + 1]
                    && f2.as_str() == "add" && a2[0] == d1
                {
                    mir.stmts[i] = MirStmt::SemiringFold {
                        op: SemiringOp::Add,
                        values: vec![*a1[0], *a1[1], *a2[1]],
                        result: *d2,
                    };
                    mir.stmts.remove(i + 1);
                    changed = true;
                    continue;
                }
            }
            i += 1;
        }
        changed
    }
}
