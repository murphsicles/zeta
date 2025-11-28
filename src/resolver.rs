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
use std::fmt;

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
    /// TimingOwned wrapper for constant-time.
    TimingOwned(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::Named(s) => write!(f, "{}", s),
            Type::Unknown => write!(f, "unknown"),
            Type::TimingOwned(ty) => write!(f, "TimingOwned<{}>", ty),
        }
    }
}

type MethodSig = (Vec<Type>, Type);
type ImplMethods = HashMap<String, MethodSig>;
type FuncSig = (Vec<(String, Type)>, Type); // params -> ret
type FuncMap = HashMap<String, FuncSig>;

/// Main resolver struct, orchestrating type checking and lowering.
#[derive(Clone)]
pub struct Resolver {
    /// Direct impls for traits on types.
    direct_impls: HashMap<(String, Type), ImplMethods>,
    /// Environment for type inference (var -> type).
    type_env: HashMap<String, Type>,
    /// Function signatures.
    func_sigs: FuncMap,
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
            func_sigs: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
        };

        // Fast-path: i64 implements Addable
        let mut addable = HashMap::new();
        addable.insert("add".to_string(), (vec![Type::I64], Type::I64));
        r.direct_impls
            .insert(("Addable".to_string(), Type::I64), addable);

        r
    }

    /// Registers a concept, impl, or func; declares param borrows.
    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ImplBlock { concept, ty, body } => {
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
            AstNode::FuncDef { name, params, ret, .. } => {
                let sig = (
                    params
                        .into_iter()
                        .map(|(n, t)| (n, self.parse_type(&t)))
                        .collect(),
                    self.parse_type(&ret),
                );
                self.func_sigs.insert(name.clone(), sig.clone());
                // Declare params in env & borrow owned
                for (pname, pty) in &sig.0 {
                    self.type_env.insert(pname.clone(), pty.clone());
                    self.borrow_checker.declare(pname.clone(), crate::borrow::BorrowState::Owned);
                }
            }
            AstNode::ConceptDef { .. } => {
                // Concepts define traits; impls register them. Concepts themselves don't add impls.
            }
            AstNode::EnumDef { name, .. } | AstNode::StructDef { name, .. } => {
                self.type_env.insert(name.clone(), Type::Named(name));
            }
            _ => {}
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
                self.borrow_checker.declare(name.clone(), crate::borrow::BorrowState::Owned);
                ty
            }
            AstNode::TimingOwned { ty: _ty_str, inner } => {
                let inner_ty = self.infer_type(inner);
                // Enforce constant-time wrapper
                Type::TimingOwned(Box::new(inner_ty))
            }
            AstNode::Call { receiver, method, args, type_args } => {
                let recv_ty = receiver.as_ref().map(|r| self.infer_type(r));
                let arg_tys: Vec<_> = args.iter().map(|a| self.infer_type(a)).collect();

                // Lookup func sig first
                if let Some(sig) = self.func_sigs.get(method) {
                    if sig.0.len() == arg_tys.len() {
                        // Check arg compat (simplified)
                        return sig.1.clone();
                    }
                }

                // Trait lookup fallback
                if let Some(recv_ty) = &recv_ty {
                    // Nominal lookup
                    if let Some(impls) = self
                        .direct_impls
                        .get(&("Addable".to_string(), recv_ty.clone()))
                    {
                        if let Some((params, ret)) = impls.get(method) {
                            if params.len() == arg_tys.len() {
                                return ret.clone();
                            }
                        }
                    }
                    // Structural: fallback for primitives
                    if method == "add" && recv_ty == &Type::I64 {
                        return Type::I64;
                    }
                }

                // Partial specialization: Check for partial match on type_args
                let key = MonoKey {
                    func_name: method.clone(),
                    type_args: type_args.clone(),
                };

                if let Some(cached) = lookup_specialization(&key) {
                    return Type::Named(cached.llvm_func_name);
                }

                // Generate mangled name for partial spec
                let recv_str = recv_ty
                    .as_ref()
                    .map_or_else(|| "unknown".to_string(), |t| t.to_string());
                let mut mangled = format!("{}_{}", method, recv_str);
                if !type_args.is_empty() {
                    mangled.push_str("__partial");
                    for t in type_args {
                        if is_cache_safe(t) {
                            mangled.push_str(&t.replace(['<', '>', ':'], "_"));
                            mangled.push('_');
                        } else {
                            mangled.push_str("dyn_");
                        }
                    }
                }

                let cache_safe =
                    type_args.iter().all(|t| is_cache_safe(t)) && recv_ty.is_some();
                record_specialization(
                    key,
                    MonoValue {
                        llvm_func_name: mangled.clone(),
                        cache_safe,
                    },
                );
                Type::Named(mangled)
            }
            _ => Type::Unknown,
        }
    }

    /// Performs type checking and borrow checking on a program.
    /// Returns true if all checks pass.
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;
        for ast in asts {
            self.register(ast.clone()); // Register concepts/impls first
            if let AstNode::FuncDef { name, body, .. } = ast {
                // Reset borrow states per fn
                self.borrow_checker = BorrowChecker::new();
                if let Some(sig) = self.func_sigs.get(name) {
                    for (pname, _) in &sig.0 {
                        self.borrow_checker.declare(pname.clone(), crate::borrow::BorrowState::Owned);
                    }
                }
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
        let mut mir = mir_gen.gen_mir(ast);
        self.fold_semiring_chains(&mut mir);
        mir
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
                    // No i += 1; check new stmt at i
                } else {
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
        changed
    }
}
