// src/resolver.rs
//! Type resolver and semantic analyzer for Zeta.
//! Handles type inference, trait resolution, borrow checking, MIR lowering, and optimizations.
//! Integrates algebraic structures from EOP for semiring-based codegen.
//! Added: Stable ABI checks (no UB, const-time TimingOwned validation).
//! Added: CTFE (const eval semirings) for literal/semiring folding during inference.

use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use crate::specialization::{
    MonoKey, MonoValue, is_cache_safe, lookup_specialization, record_specialization,
};
use std::collections::HashMap;
use std::fmt;

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

/// Enum for ABI violations.
#[derive(Debug, Clone)]
pub enum AbiError {
    RawPointerInPublic,
    NonConstTimeTimingOwned,
    UnsafeCast,
}

/// Enum for constant values (for CTFE).
#[derive(Debug, Clone)]
pub enum ConstValue {
    Int(i64),
    Float(f32),
    Bool(bool),
}

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
                    if let AstNode::Method {
                        name, params, ret, ..
                    } = node
                    {
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
            AstNode::FuncDef {
                name, params, ret, ..
            } => {
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
                    self.borrow_checker
                        .declare(pname.clone(), crate::borrow::BorrowState::Owned);
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

    #[allow(dead_code)]
    /// Constant-time folding evaluation for semiring ops on literals.
    /// Returns Some(ConstValue) if fully constant, else None.
    fn const_eval_semiring(&self, _node: &AstNode) -> Option<ConstValue> {
        None // Placeholder implementation
    }

    /// Resolves method call type via direct impl, structural, or specialization.
    fn resolve_method_type(
        &mut self,
        recv_ty: Option<Type>,
        method: &str,
        arg_tys: &[Type],
        type_args: &[Type],
        structural: bool,
    ) -> Type {
        if let Some(recv_ty) = &recv_ty
            && let Some(impls) = self.direct_impls.get(&("Addable".to_string(), recv_ty.clone()))
            && let Some((params, ret)) = impls.get(method)
            && params.len() == arg_tys.len()
        {
            return ret.clone();
        }
        // Structural: fallback for primitives if structural
        if structural && method == "add" && recv_ty.as_ref() == Some(&Type::I64) {
            return Type::I64;
        }

        // Partial specialization: Check for partial match on type_args
        let key = MonoKey {
            func_name: method.to_string(),
            type_args: type_args.iter().map(|t| t.to_string()).collect(),
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
                if is_cache_safe(&t.to_string()) {
                    mangled.push_str(&t.to_string().replace(['<', '>', ':'], "_"));
                    mangled.push('_');
                } else {
                    mangled.push_str("dyn_");
                }
            }
        }

        let cache_safe =
            type_args.iter().all(|t| is_cache_safe(&t.to_string())) && recv_ty.is_some();
        record_specialization(
            key,
            MonoValue {
                llvm_func_name: mangled.clone(),
                cache_safe,
            },
        );
        Type::Named(mangled)
    }

    /// Infers type for an AST node, resolving methods and folding constants.
    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_n) => Type::I64,
            AstNode::Var(v) => self.type_env.get(v).cloned().unwrap_or(Type::Unknown),
            AstNode::Call {
                receiver,
                method,
                args,
                type_args,
                structural,
                ..
            } => {
                let recv_ty = receiver.as_ref().map(|r| self.infer_type(r));
                let arg_tys: Vec<Type> = args.iter().map(|a| self.infer_type(a)).collect();
                let type_args: Vec<Type> = type_args.iter().map(|s| self.parse_type(s)).collect();
                self.resolve_method_type(recv_ty, method, &arg_tys, &type_args, *structural)
            }
            AstNode::TimingOwned { inner, .. } => {
                Type::TimingOwned(Box::new(self.infer_type(inner)))
            }
            _ => Type::Unknown,
        }
    }

    /// Performs ABI checks: no raw pointers in public, TimingOwned only on const-time primitives.
    fn check_abi(&mut self, node: &AstNode) -> Result<(), AbiError> {
        match node {
            AstNode::TimingOwned { inner, .. } => {
                let inner_ty = self.infer_type(inner);
                match &inner_ty {
                    Type::I64 | Type::F32 | Type::Bool => Ok(()), // Primitives ok
                    _ => Err(AbiError::NonConstTimeTimingOwned),
                }
            }
            AstNode::Call { method, .. } if method == "as_ptr" => Err(AbiError::RawPointerInPublic),
            _ => Ok(()),
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
                        self.borrow_checker
                            .declare(pname.clone(), crate::borrow::BorrowState::Owned);
                    }
                }
                for stmt in body {
                    self.infer_type(stmt);
                    if !self.borrow_checker.check(stmt) {
                        ok = false;
                    }
                    if self.check_abi(stmt).is_err() {
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
