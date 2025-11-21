// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, MirExpr, SemiringOp};
use crate::specialization::{lookup_specialization, record_specialization, MonoKey, MonoValue, is_cache_safe};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I64,
    F32,
    Bool,
    Generic(String),
    Named(String),
    Actor(String),
    Future(Box<Type>),
    Unknown,
}

#[derive(Debug, Clone)]
pub struct MethodSig {
    pub params: Vec<Type>,
    pub ret: Type,
}

#[derive(Clone)]
pub struct Resolver {
    direct_impls: HashMap<(String, Type), HashMap<String, MethodSig>>,
    cache: Arc<RwLock<HashMap<(Type, String), Option<MethodSig>>>>,
    type_env: HashMap<String, Type>,
    borrow_checker: BorrowChecker,
}

impl Resolver {
    pub fn new() -> Self {
        let mut r = Self {
            direct_impls: HashMap::new(),
            cache: Arc::new(RwLock::new(HashMap::new())),
            type_env: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
        };

        // Fast-path Addable for i64
        let mut impls = HashMap::new();
        impls.insert("add".to_string(), MethodSig {
            params: vec![Type::I64],
            ret: Type::I64,
        });
        r.direct_impls.insert(("Addable".to_string(), Type::I64), impls);

        r
    }

    pub fn register(&mut self, ast: AstNode) {
        if let AstNode::ImplBlock { concept, ty, body } = ast {
            let ty = self.parse_type(&ty);
            let mut methods = HashMap::new();
            for node in body {
                if let AstNode::Method { name, params, ret } = node {
                    let sig = MethodSig {
                        params: params.into_iter().map(|(_, t)| self.parse_type(&t)).collect(),
                        ret: self.parse_type(&ret),
                    };
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

    // Thin monomorphization + specialization cache
    pub fn resolve_call(
        &mut self,
        method: &str,
        type_args: &[String],
        receiver_ty: Option<&Type>,
    ) -> (String, bool) {
        let key = MonoKey {
            func_name: method.to_string(),
            type_args: type_args.to_vec(),
        };

        if let Some(cached) = lookup_specialization(&key) {
            return (cached.llvm_func_name, cached.cache_safe);
        }

        let mut concrete_name = method.to_string();
        let mut cache_safe = true;

        if !type_args.is_empty() {
            concrete_name.push_str("__");
            for t in type_args {
                let sanitized = t.replace("::", "_").replace(['<', '>'], "");
                concrete_name.push_str(&sanitized);
                concrete_name.push('_');
                if !is_cache_safe(t) {
                    cache_safe = false;
                }
            }
        }

        let value = MonoValue {
            llvm_func_name: concrete_name.clone(),
            cache_safe,
        };
        record_specialization(key, value);

        (concrete_name, cache_safe)
    }

    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_) => Type::I64,
            AstNode::Var(v) => self.type_env.get(v).cloned().unwrap_or(Type::Unknown),
            AstNode::Call { receiver, method, type_args, .. } => {
                let recv_ty = receiver.as_ref().map(|r| self.infer_type(r));
                let (llvm_name, _cache_safe) = self.resolve_call(method, type_args, recv_ty.as_ref());
                Type::Named(llvm_name)
            }
            AstNode::Assign(name, expr) => {
                let ty = self.infer_type(expr);
                self.type_env.insert(name.clone(), ty.clone());
                ty
            }
            _ => Type::Unknown,
        }
    }

    pub fn extract_type_args_from_context(&self) -> Vec<String> {
        // Placeholder — will be filled by generic context tracking
        vec![]
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
        let mut gen = MirGen::new();
        gen.gen_mir(ast)
    }

    pub fn fold_semiring_chains(&self, mir: &mut Mir) -> bool {
        let mut changed = false;
        let mut i = 0;
        while i < mir.stmts.len() {
            if let MirStmt::Call { func, args, dest } = &mir.stmts[i] {
                if func == "add" || func == "mul" {
                    let op = if func == "add" { SemiringOp::Add } else { SemiringOp::Mul };
                    let mut chain = vec![args[0]];
                    let mut current = *dest;
                    let mut j = i + 1;

                    while j < mir.stmts.len() {
                        if let MirStmt::Call { func: f, args: a, dest: d } = &mir.stmts[j] {
                            if f == func && a[0] == current {
                                chain.push(a[1]);
                                current = *d;
                                j += 1;
                                changed = true;
                            } else { break; }
                        } else { break; }
                    }

                    if chain.len() >= 3 {
                        mir.stmts[i] = MirStmt::SemiringFold {
                            op,
                            values: chain,
                            result: current,
                        };
                        mir.stmts.drain(i + 1..j);
                    }
                }
            }
            i += 1;
        }
        changed
    }
}
