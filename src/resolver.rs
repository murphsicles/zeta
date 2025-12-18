// src/resolver.rs
//! Type resolver and semantic analyzer for Zeta.
//! Handles type inference, trait resolution, borrow checking, MIR lowering, and optimizations.
//! Integrates algebraic structures for semiring-based codegen.
//! Ensures stable ABI and constant-time guarantees.
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
#[allow(unused_imports)]
use crate::specialization::{
    MonoKey, MonoValue, is_cache_safe, lookup_specialization, record_specialization,
};
use std::collections::HashMap;
use std::fmt;
/// Represents types in the Zeta language.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// 64-bit signed integer.
    I64,
    /// 32-bit float.
    F32,
    /// Boolean.
    Bool,
    /// Unified owned UTF-8 string type.
    Str,
    /// Borrowed string slice.
    StrRef,
    /// Named type (e.g., user-defined or trait-bound).
    Named(String),
    /// Unknown/inferred type.
    Unknown,
    /// Timing-owned wrapper for constant-time.
    TimingOwned(Box<Type>),
    /// Vec<u8> for interop.
    VecU8,
    /// Result type for error handling.
    Result(Box<Type>, Box<Type>),
    /// Map type for dicts.
    Map(Box<Type>, Box<Type>),
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::StrRef => write!(f, "&str"),
            Type::VecU8 => write!(f, "Vec<u8>"),
            Type::Named(s) => write!(f, "{}", s),
            Type::Unknown => write!(f, "unknown"),
            Type::TimingOwned(ty) => write!(f, "TimingOwned<{}>", ty),
            Type::Result(ok, err) => write!(f, "Result<{}, {}>", ok, err),
            Type::Map(k, v) => write!(f, "Map<{}, {}>", k, v),
        }
    }
}
type MethodSig = (Vec<Type>, Type);
type ImplMethods = HashMap<String, MethodSig>;
type FuncSig = (Vec<(String, Type)>, Type);
type FuncMap = HashMap<String, FuncSig>;
/// ABI violation errors.
#[derive(Debug, Clone)]
pub enum AbiError {
    RawPointerInPublic,
    NonConstTimeTimingOwned,
    UnsafeCast,
}
/// Constant values for compile-time evaluation.
#[derive(Debug, Clone)]
pub enum ConstValue {
    Int(i64),
    Float(f32),
    Bool(bool),
    Str(String),
}
impl ConstValue {
    /// Applies semiring operation to two constants.
    pub fn fold_semiring(&self, op: SemiringOp, other: &Self) -> Option<Self> {
        match (self, other, op) {
            (ConstValue::Int(a), ConstValue::Int(b), SemiringOp::Add) => {
                Some(ConstValue::Int(a + b))
            }
            (ConstValue::Int(a), ConstValue::Int(b), SemiringOp::Mul) => {
                Some(ConstValue::Int(a * b))
            }
            _ => None,
        }
    }
}
/// Coordinates type resolution, semantic checks, and MIR lowering.
#[derive(Clone)]
pub struct Resolver {
    /// Trait implementations for types.
    direct_impls: HashMap<(String, Type), ImplMethods>,
    /// Variable to type mappings.
    type_env: HashMap<String, Type>,
    /// Function signatures.
    func_sigs: FuncMap,
    /// Integrated borrow checker.
    borrow_checker: BorrowChecker,
    /// Map from (type, method) to (concept, sig) for resolution.
    trait_methods: HashMap<(Type, String), (String, MethodSig)>,
}
impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}
impl Resolver {
    /// Initializes with built-in types and implementations.
    pub fn new() -> Self {
        let mut r = Self {
            direct_impls: HashMap::new(),
            type_env: HashMap::new(),
            func_sigs: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
            trait_methods: HashMap::new(),
        };
        // Fast-path: i64 implements Addable
        let mut addable = HashMap::new();
        addable.insert("add".to_string(), (vec![Type::I64], Type::I64));
        r.direct_impls
            .insert(("Addable".to_string(), Type::I64), addable);
        r.trait_methods.insert((Type::I64, "add".to_string()), ("Addable".to_string(), (vec![Type::I64], Type::I64)));
        // Builtin StrOps for unified strings - expanded
        let mut str_ops = HashMap::new();
        str_ops.insert("len".to_string(), (vec![], Type::I64));
        str_ops.insert("concat".to_string(), (vec![Type::Str], Type::Str));
        str_ops.insert("contains".to_string(), (vec![Type::Str], Type::Bool));
        str_ops.insert("trim".to_string(), (vec![], Type::Str));
        str_ops.insert(
            "split".to_string(),
            (vec![Type::Str], Type::Named("Vec<str>".to_string())),
        );
        // Rich methods
        str_ops.insert("to_lowercase".to_string(), (vec![], Type::Str));
        str_ops.insert(
            "replace".to_string(),
            (vec![Type::Str, Type::Str], Type::Str),
        );
        str_ops.insert("starts_with".to_string(), (vec![Type::Str], Type::Bool));
        str_ops.insert("ends_with".to_string(), (vec![Type::Str], Type::Bool));
        str_ops.insert("as_bytes".to_string(), (vec![], Type::VecU8));
        r.direct_impls
            .insert(("StrOps".to_string(), Type::Str), str_ops);
        // Insert into trait_methods
        r.trait_methods.insert((Type::Str, "len".to_string()), ("StrOps".to_string(), (vec![], Type::I64)));
        r.trait_methods.insert((Type::Str, "concat".to_string()), ("StrOps".to_string(), (vec![Type::Str], Type::Str)));
        r.trait_methods.insert((Type::Str, "contains".to_string()), ("StrOps".to_string(), (vec![Type::Str], Type::Bool)));
        r.trait_methods.insert((Type::Str, "trim".to_string()), ("StrOps".to_string(), (vec![], Type::Str)));
        r.trait_methods.insert((Type::Str, "split".to_string()), ("StrOps".to_string(), (vec![Type::Str], Type::Named("Vec<str>".to_string()))));
        r.trait_methods.insert((Type::Str, "to_lowercase".to_string()), ("StrOps".to_string(), (vec![], Type::Str)));
        r.trait_methods.insert((Type::Str, "replace".to_string()), ("StrOps".to_string(), (vec![Type::Str, Type::Str], Type::Str)));
        r.trait_methods.insert((Type::Str, "starts_with".to_string()), ("StrOps".to_string(), (vec![Type::Str], Type::Bool)));
        r.trait_methods.insert((Type::Str, "ends_with".to_string()), ("StrOps".to_string(), (vec![Type::Str], Type::Bool)));
        r.trait_methods.insert((Type::Str, "as_bytes".to_string()), ("StrOps".to_string(), (vec![], Type::VecU8)));
        r
    }
    /// Registers definitions for resolution.
    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { methods, .. } => {
                // Concepts don't need registration beyond methods; sigs checked in impls
                for _m in methods {
                    // Potential: store concept methods for validation
                }
            }
            AstNode::ImplBlock { concept, ty, body } => {
                let ty = Type::Named(ty);
                let mut methods = HashMap::new();
                for m in body {
                    if let AstNode::Method { name, params, ret, .. } = m {
                        let ptypes: Vec<Type> = params.iter().map(|(_, t)| Type::Named(t.clone())).collect();
                        let ret_ty = Type::Named(ret);
                        methods.insert(name.clone(), (ptypes.clone(), ret_ty.clone()));
                        self.trait_methods.insert((ty.clone(), name), (concept.clone(), (ptypes, ret_ty)));
                    }
                }
                self.direct_impls.insert((concept, ty), methods);
            }
            AstNode::FuncDef { name, params, ret, .. } => {
                let ptypes: Vec<(String, Type)> = params.iter().map(|(n, t)| (n.clone(), Type::Named(t.clone()))).collect();
                self.func_sigs.insert(name, (ptypes, Type::Named(ret)));
            }
            AstNode::EnumDef { name, .. } => {
                self.type_env.insert(name, Type::Named("enum".to_string())); // Stub
            }
            AstNode::StructDef { name, .. } => {
                self.type_env.insert(name, Type::Named("struct".to_string())); // Stub
            }
            _ => {}
        }
    }
    /// Infers the type of an AST node.
    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_) => Type::I64,
            AstNode::StringLit(_) => Type::Str,
            AstNode::FString(_) => Type::Str,
            AstNode::Var(name) => self.type_env.get(name).cloned().unwrap_or(Type::Unknown),
            AstNode::BinaryOp { op, left, right } => {
                let lty = self.infer_type(left);
                let rty = self.infer_type(right);
                if op == "+" || op == "-" || op == "*" || op == "/" {
                    if lty == Type::I64 && rty == Type::I64 {
                        Type::I64
                    } else {
                        Type::Unknown
                    }
                } else if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
                    Type::Bool
                } else {
                    Type::Unknown
                }
            }
            AstNode::Call {
                receiver,
                method,
                args,
                ..
            } => {
                if let Some(rec) = receiver {
                    let rec_ty = self.infer_type(rec);
                    if let Some((_, sig)) = self.trait_methods.get(&(rec_ty.clone(), method.clone())) {
                        sig.1.clone()
                    } else {
                        Type::Unknown
                    }
                } else {
                    // Static func call
                    if let Some(sig) = self.func_sigs.get(method) {
                        sig.1.clone()
                    } else {
                        Type::Unknown
                    }
                }
            }
            AstNode::Spawn { .. } => Type::I64, // Actor ID
            AstNode::TimingOwned { inner, .. } => {
                let inner_ty = self.infer_type(inner);
                Type::TimingOwned(Box::new(inner_ty))
            }
            AstNode::Defer(_) => Type::Unknown,
            AstNode::TryProp { expr } => self.infer_type(expr),
            AstNode::DictLit { entries } => {
                if entries.is_empty() {
                    Type::Map(Box::new(Type::Unknown), Box::new(Type::Unknown))
                } else {
                    let k_ty = self.infer_type(&entries[0].0);
                    let v_ty = self.infer_type(&entries[0].1);
                    // Assume all entries match; in full check, verify
                    Type::Map(Box::new(k_ty), Box::new(v_ty))
                }
            }
            AstNode::Subscript { base, index } => {
                let base_ty = self.infer_type(base);
                let index_ty = self.infer_type(index);
                if let Type::Map(k, v) = base_ty {
                    if index_ty == *k { *v } else { Type::Unknown }
                } else {
                    Type::Unknown
                }
            }
            AstNode::Return(inner) => self.infer_type(inner),
            _ => Type::Unknown,
        }
    }
    /// Checks ABI compliance for a node.
    fn check_abi(&mut self, node: &AstNode) -> Result<(), AbiError> {
        match node {
            AstNode::TimingOwned { inner, .. } => {
                let inner_ty = self.infer_type(inner);
                match &inner_ty {
                    Type::I64 | Type::F32 | Type::Bool | Type::Str => Ok(()),
                    _ => Err(AbiError::NonConstTimeTimingOwned),
                }
            }
            AstNode::Call { method, .. } if method == "as_ptr" => Err(AbiError::RawPointerInPublic),
            _ => Ok(()),
        }
    }
    /// Applies implicit borrowing for compatible types.
    #[allow(dead_code)]
    fn implicit_borrow(&mut self, ty: &Type) -> Type {
        if *ty == Type::Str {
            Type::StrRef // Implicit &str borrow
        } else {
            ty.clone()
        }
    }
    /// Detects error propagation (?) in a node.
    fn has_try_prop(&self, node: &AstNode) -> bool {
        match node {
            AstNode::TryProp { .. } => true,
            AstNode::BinaryOp { left, right, .. } => {
                self.has_try_prop(left) || self.has_try_prop(right)
            }
            AstNode::Call { args, receiver, .. } => {
                receiver.as_ref().is_some_and(|r| self.has_try_prop(r))
                    || args.iter().any(|a| self.has_try_prop(a))
            }
            AstNode::Return(inner) => self.has_try_prop(inner),
            _ => false,
        }
    }
    /// Typechecks and borrow-checks the program.
    /// Returns true if valid.
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;
        for ast in asts {
            self.register(ast.clone());
            if let AstNode::FuncDef { name, body, .. } = ast {
                self.borrow_checker = BorrowChecker::new();
                if let Some(sig) = self.func_sigs.get(name) {
                    for (pname, _) in &sig.0 {
                        self.borrow_checker
                            .declare(pname.clone(), crate::borrow::BorrowState::Owned);
                    }
                    let fn_ret = &sig.1;
                    let has_prop = body.iter().any(|stmt| self.has_try_prop(stmt));
                    if has_prop {
                        if let Type::Result(_, _) = fn_ret {
                        } else {
                            ok = false;
                        } // Require Result ret if ? used
                    }
                }
                for stmt in body {
                    let ty = self.infer_type(stmt);
                    self.type_env.insert("temp".to_string(), ty); // Temp for checks
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
    /// Lowers AST to MIR.
    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = MirGen::new();
        let mut mir = mir_gen.gen_mir(ast);
        self.fold_semiring_chains(&mut mir);
        mir
    }
    /// Folds chains of semiring operations in MIR.
    /// Returns true if modified.
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
