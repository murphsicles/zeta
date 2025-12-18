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
    /// Named type (e.g., primitives, user-defined, or parameterized like Result<T,E>).
    Named { name: String, params: Vec<Type> },
    /// Unknown/inferred type.
    Unknown,
    /// Timing-owned wrapper for constant-time.
    TimingOwned(Box<Type>),
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Named { name, params } if params.is_empty() => write!(f, "{}", name),
            Type::Named { name, params } => {
                write!(f, "{}<", name)?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ">")
            }
            Type::Unknown => write!(f, "unknown"),
            Type::TimingOwned(ty) => write!(f, "TimingOwned<{}>", ty),
        }
    }
}
impl Type {
    /// Creates a primitive type.
    pub fn primitive(name: &str) -> Self {
        Self::Named {
            name: name.to_string(),
            params: vec![],
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
        let i64_ty = Type::primitive("i64");
        let bool_ty = Type::primitive("bool");
        let str_ty = Type::primitive("str");
        let vec_u8_ty = Type::Named { name: "Vec".to_string(), params: vec![Type::primitive("u8")] };
        // Fast-path: i64 implements Addable
        let mut addable = HashMap::new();
        addable.insert("add".to_string(), (vec![i64_ty.clone()], i64_ty.clone()));
        r.direct_impls
            .insert(("Addable".to_string(), i64_ty.clone()), addable);
        r.trait_methods.insert((i64_ty.clone(), "add".to_string()), ("Addable".to_string(), (vec![i64_ty.clone()], i64_ty.clone())));
        // Builtin StrOps for unified strings - expanded
        let mut str_ops = HashMap::new();
        str_ops.insert("len".to_string(), (vec![], i64_ty.clone()));
        str_ops.insert("concat".to_string(), (vec![str_ty.clone()], str_ty.clone()));
        str_ops.insert("contains".to_string(), (vec![str_ty.clone()], bool_ty.clone()));
        str_ops.insert("trim".to_string(), (vec![], str_ty.clone()));
        let vec_str_ty = Type::Named { name: "Vec".to_string(), params: vec![str_ty.clone()] };
        str_ops.insert("split".to_string(), (vec![str_ty.clone()], vec_str_ty));
        // Rich methods
        str_ops.insert("to_lowercase".to_string(), (vec![], str_ty.clone()));
        str_ops.insert(
            "replace".to_string(),
            (vec![str_ty.clone(), str_ty.clone()], str_ty.clone()),
        );
        str_ops.insert("starts_with".to_string(), (vec![str_ty.clone()], bool_ty.clone()));
        str_ops.insert("ends_with".to_string(), (vec![str_ty.clone()], bool_ty.clone()));
        str_ops.insert("as_bytes".to_string(), (vec![], vec_u8_ty));
        r.direct_impls
            .insert(("StrOps".to_string(), str_ty.clone()), str_ops);
        // Insert into trait_methods
        r.trait_methods.insert((str_ty.clone(), "len".to_string()), ("StrOps".to_string(), (vec![], i64_ty.clone())));
        r.trait_methods.insert((str_ty.clone(), "concat".to_string()), ("StrOps".to_string(), (vec![str_ty.clone()], str_ty.clone())));
        r.trait_methods.insert((str_ty.clone(), "contains".to_string()), ("StrOps".to_string(), (vec![str_ty.clone()], bool_ty.clone())));
        r.trait_methods.insert((str_ty.clone(), "trim".to_string()), ("StrOps".to_string(), (vec![], str_ty.clone())));
        r.trait_methods.insert((str_ty.clone(), "split".to_string()), ("StrOps".to_string(), (vec![str_ty.clone()], Type::Named { name: "Vec".to_string(), params: vec![str_ty.clone()] })));
        r.trait_methods.insert((str_ty.clone(), "to_lowercase".to_string()), ("StrOps".to_string(), (vec![], str_ty.clone())));
        r.trait_methods.insert((str_ty.clone(), "replace".to_string()), ("StrOps".to_string(), (vec![str_ty.clone(), str_ty.clone()], str_ty.clone())));
        r.trait_methods.insert((str_ty.clone(), "starts_with".to_string()), ("StrOps".to_string(), (vec![str_ty.clone()], bool_ty.clone())));
        r.trait_methods.insert((str_ty.clone(), "ends_with".to_string()), ("StrOps".to_string(), (vec![str_ty.clone()], bool_ty.clone())));
        r.trait_methods.insert((str_ty, "as_bytes".to_string()), ("StrOps".to_string(), (vec![], Type::Named { name: "Vec".to_string(), params: vec![Type::primitive("u8")] })));
        r
    }
    /// Determines if a type is Copy.
    pub fn is_copy(&self, ty: &Type) -> bool {
        if let Type::Named { name, params } = ty {
            if params.is_empty() {
                matches!(name.as_str(), "i64" | "f32" | "bool")
            } else {
                false
            }
        } else {
            false
        }
    }
    /// Parses a type string to a Type.
    pub fn parse_type_str(&self, s: &str) -> Type {
        let trimmed = s.trim();
        if !trimmed.contains('<') {
            return Type::primitive(trimmed);
        }
        if let Some(open) = trimmed.find('<') {
            let name = trimmed[0..open].trim().to_string();
            let inner = &trimmed[open + 1..trimmed.len() - 1];
            let param_strs: Vec<String> = inner
                .split(',')
                .map(|p| p.trim().to_string())
                .collect();
            let params: Vec<Type> = param_strs
                .iter()
                .map(|ps| self.parse_type_str(ps))
                .collect();
            Type::Named { name, params }
        } else {
            Type::Unknown
        }
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
                let ty = self.parse_type_str(&ty);
                let mut methods = HashMap::new();
                for m in body {
                    if let AstNode::Method { name, params, ret, .. } = m {
                        let ptypes: Vec<Type> = params.iter().map(|(_, t)| self.parse_type_str(t)).collect();
                        let ret_ty = self.parse_type_str(&ret);
                        methods.insert(name.clone(), (ptypes.clone(), ret_ty.clone()));
                        self.trait_methods.insert((ty.clone(), name), (concept.clone(), (ptypes, ret_ty)));
                    }
                }
                self.direct_impls.insert((concept, ty), methods);
            }
            AstNode::FuncDef { name, params, ret, .. } => {
                let ptypes: Vec<(String, Type)> = params.iter().map(|(n, t)| (n.clone(), self.parse_type_str(t))).collect();
                self.func_sigs.insert(name, (ptypes, self.parse_type_str(&ret)));
            }
            AstNode::EnumDef { name, .. } => {
                self.type_env.insert(name, Type::Named { name: "enum".to_string(), params: vec![] }); // Stub
            }
            AstNode::StructDef { name, .. } => {
                self.type_env.insert(name, Type::Named { name: "struct".to_string(), params: vec![] }); // Stub
            }
            _ => {}
        }
    }
    /// Collects used specializations from calls.
    pub fn collect_used_specializations(&self, asts: &[AstNode]) -> HashMap<String, Vec<Vec<String>>> {
        let mut used = HashMap::new();
        for ast in asts {
            if let AstNode::FuncDef { body, .. } = ast {
                for stmt in body {
                    self.collect_from_node(stmt, &mut used);
                }
            }
        }
        used
    }
    /// Helper to collect from node recursively.
    fn collect_from_node(&self, node: &AstNode, used: &mut HashMap<String, Vec<Vec<String>>>) {
        match node {
            AstNode::Call { method, type_args, .. } if !type_args.is_empty() => {
                used.entry(method.clone())
                    .or_insert(vec![])
                    .push(type_args.clone());
            }
            AstNode::BinaryOp { left, right, .. } => {
                self.collect_from_node(left, used);
                self.collect_from_node(right, used);
            }
            AstNode::FString(parts) => {
                for p in parts {
                    self.collect_from_node(p, used);
                }
            }
            AstNode::TimingOwned { inner, .. } => self.collect_from_node(inner, used),
            AstNode::Defer(inner) => self.collect_from_node(inner, used),
            AstNode::TryProp { expr, .. } => self.collect_from_node(expr, used),
            AstNode::DictLit { entries } => {
                for (k, v) in entries {
                    self.collect_from_node(k, used);
                    self.collect_from_node(v, used);
                }
            }
            AstNode::Subscript { base, index, .. } => {
                self.collect_from_node(base, used);
                self.collect_from_node(index, used);
            }
            AstNode::Return(inner) => self.collect_from_node(inner, used),
            AstNode::Assign(_, rhs) => self.collect_from_node(rhs, used),
            _ => {}
        }
    }
    /// Infers the type of an AST node.
    pub fn infer_type(&self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_) => Type::primitive("i64"),
            AstNode::StringLit(_) => Type::primitive("str"),
            AstNode::FString(_) => Type::primitive("str"),
            AstNode::Var(name) => self.type_env.get(name).cloned().unwrap_or(Type::Unknown),
            AstNode::BinaryOp { op, left, right } => {
                let lty = self.infer_type(left);
                let rty = self.infer_type(right);
                let i64_ty = Type::primitive("i64");
                let bool_ty = Type::primitive("bool");
                if op == "+" || op == "-" || op == "*" || op == "/" {
                    if lty == i64_ty && rty == i64_ty {
                        i64_ty
                    } else {
                        Type::Unknown
                    }
                } else if op == "==" || op == "!=" || op == "<" || op == ">" || op == "<=" || op == ">=" {
                    bool_ty
                } else {
                    Type::Unknown
                }
            }
            AstNode::Call {
                receiver,
                method,
                args: _,
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
            AstNode::Spawn { .. } => Type::primitive("i64"), // Actor ID
            AstNode::TimingOwned { inner, .. } => {
                let inner_ty = self.infer_type(inner);
                Type::TimingOwned(Box::new(inner_ty))
            }
            AstNode::Defer(_) => Type::Unknown,
            AstNode::TryProp { expr } => {
                let expr_ty = self.infer_type(expr);
                if let Type::Named { name, params } = &expr_ty {
                    if name == "Result" && params.len() == 2 {
                        params[0].clone()
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            }
            AstNode::DictLit { entries } => {
                if entries.is_empty() {
                    Type::Named {
                        name: "Map".to_string(),
                        params: vec![Type::Unknown, Type::Unknown],
                    }
                } else {
                    let k_ty = self.infer_type(&entries[0].0);
                    let v_ty = self.infer_type(&entries[0].1);
                    // Assume all entries match; in full check, verify
                    Type::Named {
                        name: "Map".to_string(),
                        params: vec![k_ty, v_ty],
                    }
                }
            }
            AstNode::Subscript { base, index } => {
                let base_ty = self.infer_type(base);
                let index_ty = self.infer_type(index);
                if let Type::Named { name, params } = base_ty {
                    if name == "Map" && params.len() == 2 && index_ty == params[0] {
                        params[1].clone()
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            }
            AstNode::Return(inner) => self.infer_type(inner),
            AstNode::Assign(_, rhs) => self.infer_type(rhs),
            _ => Type::Unknown,
        }
    }
    /// Checks ABI compliance for a node.
    fn check_abi(&mut self, node: &AstNode) -> Result<(), AbiError> {
        match node {
            AstNode::TimingOwned { inner, .. } => {
                let inner_ty = self.infer_type(inner);
                match inner_ty {
                    ty if ty == Type::primitive("i64") || ty == Type::primitive("f32") || ty == Type::primitive("bool") || ty == Type::primitive("str") => Ok(()),
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
        let str_ty = Type::primitive("str");
        let str_ref_ty = Type::primitive("&str");
        if *ty == str_ty {
            str_ref_ty // Implicit &str borrow
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
                    for (pname, pty) in &sig.0 {
                        self.borrow_checker
                            .declare(pname.clone(), crate::borrow::BorrowState::Owned, pty.clone());
                    }
                    let fn_ret = &sig.1;
                    let has_prop = body.iter().any(|stmt| self.has_try_prop(stmt));
                    if has_prop {
                        if let Type::Named { name, params } = fn_ret {
                            if name == "Result" && params.len() == 2 {
                            } else {
                                ok = false;
                            }
                        } else {
                            ok = false;
                        } // Require Result ret if ? used
                    }
                }
                for stmt in body {
                    let ty = self.infer_type(stmt);
                    // Create a temporary clone or extract needed data before the mutable borrow
                    let can_proceed = {
                        // Clone self to avoid the borrow conflict
                        let resolver_clone = self.clone();
                        self.borrow_checker.check(stmt, &resolver_clone)
                    };
                    if !can_proceed {
                        ok = false;
                    }
                    if self.check_abi(stmt).is_err() {
                        ok = false;
                    }
                    // Insert local types to env after check
                    if let AstNode::Assign(lhs, _) = stmt {
                        if let AstNode::Var(ref v) = **lhs {
                            self.type_env.insert(v.clone(), ty);
                        }
                    }
                }
            }
        }
        ok
    }
    /// Lowers AST to MIR.
    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = MirGen::new(self);
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
                ..
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
                    ..
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
