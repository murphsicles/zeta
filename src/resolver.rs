// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    Generic(String),
    Unknown,
}

#[derive(Debug)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
    type_env: HashMap<String, Type>,
    borrow_checker: BorrowChecker,
}

impl Resolver {
    pub fn new() -> Self {
        let mut r = Self {
            concepts: HashMap::new(),
            impls: HashMap::new(),
            funcs: HashMap::new(),
            type_env: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
        };

        // Generic Addable<T>
        r.register(AstNode::ConceptDef {
            name: "Addable".to_string(),
            params: vec!["T".to_string()],
            methods: vec![AstNode::Method {
                name: "add".to_string(),
                params: vec![("rhs".to_string(), "T".to_string())],
                ret: "T".to_string(),
            }],
        });

        // i32 implements Addable<i32>
        r.register(AstNode::ImplBlock {
            concept: "Addable".to_string(),
            ty: "i32".to_string(),
            body: vec![],
        });

        r
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { ref name, .. } => {
                self.concepts.insert(name.clone(), ast);
            }
            AstNode::ImplBlock { ref concept, ref ty, .. } => {
                self.impls.insert((concept.clone(), ty.clone()), ast);
            }
            AstNode::FuncDef { ref name, .. } => {
                self.funcs.insert(name.clone(), ast);
            }
            _ => {}
        }
    }

    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_) => Type::I32,
            AstNode::Var(v) => self.type_env.get(v).cloned().unwrap_or(Type::Unknown),
            AstNode::Call { receiver, method, args, .. } if method == "add" => {
                let recv_ty = self.infer_type(receiver);
                if let Type::I32 = recv_ty {
                    if args.len() == 1 {
                        let arg_ty = self.infer_type(&args[0]);
                        if arg_ty == Type::I32 {
                            return Type::I32;
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

    pub fn resolve_method(&self, recv_ty: &Type, method: &str) -> bool {
        matches!((recv_ty, method), (Type::I32, "add"))
            && self.impls.contains_key(&("Addable".to_string(), "i32".to_string()))
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
                if !self.borrow_checker.validate_affine(body) {
                    ok = false;
                }
                if !self.borrow_checker.validate_speculative(body) {
                    ok = false;
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
