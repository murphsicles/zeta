// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use chalk_ir::{
    interner::Interner, 
    Ty, TraitId, Goal, Program, DomainGoal, WhereClause, TraitRef,
};
use chalk_solve::{
    RustIrDatabase, Solver, Solution, Substitution, Guidance,
    solve::SLG,
};
use chalk_recursive::RecursiveSolver;
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    Generic(String),
    Named(String),
    Unknown,
}

#[derive(Debug, Clone)]
pub struct MethodSig {
    pub ret: Type,
}

#[derive(Clone)]
pub struct Resolver {
    direct_impls: HashMap<(String, Type), MethodSig>,
    chalk_db: Arc<ChalkDatabase>,
    cache: Arc<RwLock<HashMap<(Type, String), Option<MethodSig>>>>,
    borrow_checker: BorrowChecker,
}

struct ChalkDatabase {
    program: Program,
}

impl Resolver {
    pub fn new() -> Self {
        let program = build_program();
        let db = Arc::new(ChalkDatabase { program });

        let mut r = Self {
            direct_impls: HashMap::new(),
            chalk_db: db,
            cache: Arc::new(RwLock::new(HashMap::new())),
            borrow_checker: BorrowChecker::new(),
        };

        // Fast-path i32 Addable
        r.direct_impls.insert(
            ("add".to_string(), Type::I32),
            MethodSig { ret: Type::I32 },
        );

        r
    }

    pub fn register(&mut self, ast: AstNode) {
        if let AstNode::ImplBlock { concept: _, ty, body } = ast {
            for node in body {
                if let AstNode::Method { name, ret, .. } = node {
                    let ty = if ty == "i32" { Type::I32 } else { Type::Named(ty) };
                    self.direct_impls.insert(
                        (name.clone(), ty),
                        MethodSig { ret: Type::I32 },
                    );
                }
            }
        }
    }

    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(_) => Type::I32,
            AstNode::Var(_) => Type::Unknown,
            AstNode::Call { receiver, method, args, .. } => {
                let recv_ty = self.infer_type(receiver);

                // Fast path
                if let Some(sig) = self.direct_impls.get(&(method.clone(), recv_ty.clone())) {
                    return sig.ret.clone();
                }

                // Parallel Chalk++ lookup
                if let Some(sig) = self.chalk_lookup(&recv_ty, method) {
                    return sig.ret.clone();
                }

                // Semiring fallback
                if method == "add" && args.len() == 1 {
                    let arg_ty = self.infer_type(&args[0]);
                    if recv_ty == arg_ty {
                        return recv_ty;
                    }
                }

                Type::Unknown
            }
            AstNode::Assign(_, expr) => self.infer_type(expr),
            _ => Type::Unknown,
        }
    }

    fn chalk_lookup(&self, ty: &Type, method: &str) -> Option<MethodSig> {
        let key = (ty.clone(), method.to_string());
        {
            let cache = self.cache.read().unwrap();
            if let Some(cached) = cache.get(&key) {
                return cached.clone();
            }
        }

        let goal = self.build_goal(ty, method);
        let db = self.chalk_db.clone();
        let cache = self.cache.clone();

        let result = rayon::spawn(move || {
            let solver = RecursiveSolver::new(&*db);
            match solver.solve(&goal) {
                Some(Solution::Unique(_subst)) => Some(MethodSig { ret: Type::I32 }),
                _ => None,
            }
        }).join();

        cache.write().unwrap().insert(key, result.clone());
        result
    }

    fn build_goal(&self, ty: &Type, method: &str) -> Goal {
        // Simplified: just return a dummy goal
        Goal::cannot_prove()
    }

    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        asts.iter().all(|ast| {
            if let AstNode::FuncDef { body, .. } = ast {
                body.iter().all(|stmt| {
                    self.infer_type(stmt);
                    self.borrow_checker.check(stmt)
                })
            } else {
                true
            }
        })
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
                    let mut cur = *dest;
                    let mut j = i + 1;
                    while j < mir.stmts.len() {
                        if let MirStmt::Call { func: f, args: a, dest: d } = &mir.stmts[j] {
                            if f == "add" && a[0] == cur {
                                chain.push(a[1]);
                                cur = *d;
                                j += 1;
                                changed = true;
                            } else { break; }
                        } else { break; }
                    }
                    if chain.len() >= 3 {
                        mir.stmts[i] = MirStmt::SemiringFold {
                            op: SemiringOp::Add,
                            values: chain,
                            result: cur,
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

fn build_program() -> Program {
    Program::empty()
}

impl RustIrDatabase for ChalkDatabase {
    fn program(&self) -> &Program {
        &self.program
    }
}
