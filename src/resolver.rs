// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    F32,
    Bool,
    Generic(String),
    Named(String),
    Struct { name: String, fields: HashMap<String, Type> },
    Actor(String),
    Future(Box<Type>),
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Concept {
    pub name: String,
    pub params: Vec<String>,
    pub methods: HashMap<String, MethodSig>,
}

#[derive(Debug, Clone)]
pub struct MethodSig {
    pub params: Vec<Type>,
    pub ret: Type,
}

#[derive(Debug)]
pub struct Resolver {
    // Nominal concepts
    concepts: HashMap<String, Concept>,
    // Impl blocks: (concept, type) -> methods
    impls: HashMap<(String, Type), HashMap<String, MethodSig>>,
    // Structural impls: method name -> (receiver fields, sig)
    structural_impls: HashMap<String, HashMap<HashMap<String, Type>, MethodSig>>,
    // Type environment
    type_env: HashMap<String, Type>,
    borrow_checker: BorrowChecker,
    // Specialization cache
    specializations: HashMap<(String, Vec<Type>), AstNode>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut r = Self {
            concepts: HashMap::new(),
            impls: HashMap::new(),
            structural_impls: HashMap::new(),
            type_env: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
            specializations: HashMap::new(),
        };

        // Built-in Addable
        let mut addable = Concept {
            name: "Addable".to_string(),
            params: vec!["T".to_string()],
            methods: HashMap::new(),
        };
        addable.methods.insert(
            "add".to_string(),
            MethodSig {
                params: vec![Type::Generic("T".to_string())],
                ret: Type::Generic("T".to_string()),
            },
        );
        r.concepts.insert("Addable".to_string(), addable);

        // i32 impl Addable<i32>
        let mut impl_block = HashMap::new();
        impl_block.insert(
            "add".to_string(),
            MethodSig {
                params: vec![Type::I32],
                ret: Type::I32,
            },
        );
        r.impls.insert(("Addable".to_string(), Type::I32), impl_block);

        // Structural impl for any { x: i32, y: i32 } -> add -> i32
        let mut fields = HashMap::new();
        fields.insert("x".to_string(), Type::I32);
        fields.insert("y".to_string(), Type::I32);
        let mut structural = HashMap::new();
        structural.insert(
            fields,
            MethodSig {
                params: vec![Type::Struct { name: "".to_string(), fields: HashMap::new() }],
                ret: Type::I32,
            },
        );
        r.structural_impls.insert("add".to_string(), structural);

        r
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { name, params, methods } => {
                let mut concept = Concept {
                    name: name.clone(),
                    params,
                    methods: HashMap::new(),
                };
                for m in methods {
                    if let AstNode::Method { name, params, ret } = m {
                        let sig = MethodSig {
                            params: params.into_iter().map(|(_, t)| self.parse_type(&t)).collect(),
                            ret: self.parse_type(&ret),
                        };
                        concept.methods.insert(name, sig);
                    }
                }
                self.concepts.insert(name, concept);
            }
            AstNode::ImplBlock { concept, ty, body } => {
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
                self.impls.insert((concept, ty), methods);
            }
            AstNode::FuncDef { name, .. } => {
                self.type_env.insert(name, Type::Unknown);
            }
            _ => {}
        }
    }

    fn parse_type(&self, s: &str) -> Type {
        match s {
            "i32" => Type::I32,
            "f32" => Type::F32,
            "bool" => Type::Bool,
            _ => Type::Named(s.to_string()),
        }
    }

    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(n) if *n >= -2147483648 && *n <= 2147483647 => Type::I32,
            AstNode::Lit(_) => Type::Unknown,
            AstNode::Var(v) => self.type_env.get(v).cloned().unwrap_or(Type::Unknown),
            AstNode::Call { receiver, method, args, .. } => {
                let recv_ty = self.infer_type(receiver);

                // 1. Nominal lookup
                if let Some(impls) = self.lookup_nominal_impl(&recv_ty, method) {
                    if let Some(sig) = impls.get(method) {
                        return sig.ret.clone();
                    }
                }

                // 2. Structural lookup
                if let Type::Struct { fields, .. } = &recv_ty {
                    if let Some(structural) = self.structural_impls.get(method) {
                        if structural.contains_key(fields) {
                            return structural[fields].ret.clone();
                        }
                    }
                }

                // 3. Partial specialization fallback
                if method == "add" {
                    if args.len() == 1 {
                        let arg_ty = self.infer_type(&args[0]);
                        if recv_ty == arg_ty {
                            return recv_ty;
                        }
                    }
                }

                Type::Unknown
            }
            AstNode::Assign(name, expr) => {
                let ty = self.infer_type(expr);
                self.type_env.insert(name.clone(), ty.clone());
                ty
            }
            _ => Type::Unknown,
        }
    }

    fn lookup_nominal_impl(&self, ty: &Type, method: &str) -> Option<&HashMap<String, MethodSig>> {
        for (concept_name, concept) in &self.concepts {
            if let Some(impls) = self.impls.get(&(concept_name.clone(), ty.clone())) {
                if impls.contains_key(method) {
                    return Some(impls);
                }
            }
        }
        None
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
        // unchanged
        let mut changed = false;
        let mut i = 0;
        while i < mir.stmts.len() {
            if let MirStmt::Call { func, args, dest } = &mir.stmts[i] {
                if func == "add" {
                    let mut chain = vec![args[0]];
                    let mut current = *dest;
                    let mut j = i + 1;
                    while j < mir.stmts.len() {
                        if let MirStmt::Call { func: f, args: a, dest: d } = &mir.stmts[j] {
                            if f == "add" && a[0] == current {
                                chain.push(a[1]);
                                current = *d;
                                j += 1;
                                changed = true;
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    if chain.len() >= 3 {
                        mir.stmts[i] = MirStmt::SemiringFold {
                            op: SemiringOp::Add,
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
