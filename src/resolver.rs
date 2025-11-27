// src/resolver.rs
//! Type resolver and semantic analyzer for Zeta.
//! Handles type inference, trait resolution, borrow checking, MIR lowering, and optimizations.
//! Integrates algebraic structures from EOP for semiring-based codegen.

use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use crate::specialization::{
    MonoKey, MonoValue, is_cache_safe, lookup_specialization, record_specialization,
};
use std::collections::HashMap;

/// Enum for Zeta types, supporting primitives and named types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// 64-bit signed integer.
    I64,
    /// 32-bit float.
    F32,
    /// Boolean.
    Bool,
    /// Named type (e.g., user-defined or trait-bound).
    Named(String),
    /// Unknown/inferred type.
    Unknown,
}

type MethodSig = (Vec<Type>, Type);
type ImplMethods = HashMap<String, MethodSig>;

/// Main resolver struct, orchestrating type checking and lowering.
#[derive(Clone)]
pub struct Resolver {
    /// Direct impls for traits on types.
    direct_impls: HashMap<(String, Type), ImplMethods>,
    /// Environment for type inference (var -> type).
    type_env: HashMap<String, Type>,
    /// Integrated borrow checker.
    borrow_checker: BorrowChecker,
}

impl Default for Resolver {
    /// Default resolver with builtin Addable for i64.
    fn default() -> Self {
        Self::new()
    }
}

impl Resolver {
    /// Creates a new resolver with builtin fast-path for i64 Addable.
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

    /// Registers an impl block into the direct impls map.
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

    /// Parses a string to a Type variant.
    fn parse_type(&self, s: &str) -> Type {
        match s {
            "i64" => Type::I64,
            "f32" => Type::F32,
            "bool" => Type::Bool,
            _ => Type::Named(s.to_string()),
        }
    }

    /// Infers the type of an AST node, updating the environment.
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
                    && let Some(impls) = self
                        .direct_impls
                        .get(&("Addable".to_string(), recv_ty.clone()))
                    && let Some((params, ret)) = impls.get(method)
                    && params.len() == args.len()
                {
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

    /// Performs type checking and borrow checking on a program.
    /// Returns true if all checks pass.
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

    /// Lowers an AST node to MIR for codegen.
    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = MirGen::new();
        mir_gen.gen_mir(ast)
    }

    /// Optimizes MIR by folding consecutive semiring operations (e.g., add chains).
    /// Returns true if any folds occurred.
    pub fn fold_semiring_chains(&self, mir: &mut Mir) -> bool {
        let mut changed = false;
        let mut i = 0;
        while i + 1 < mir.stmts.len() {
            if let MirStmt::Call {
                func: f1,
                args: a1,
                dest: d1,
            } = &mir.stmts[i]
            {
                if f1.as_str() != "add" {
                    i += 1;
                    continue;
                }
                if let MirStmt::Call {
                    func: f2,
                    args: a2,
                    dest: d2,
                } = &mir.stmts[i + 1]
                    && f2.as_str() == "add"
                    && a2[0] == *d1
                {
                    mir.stmts[i] = MirStmt::SemiringFold {
                        op: SemiringOp::Add,
                        values: vec![a1[0], a1[1], a2[1]],
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
