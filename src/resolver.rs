// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, MirExpr, SemiringOp};
use chalk_ir::{
    interner::{Interner, Interned},
    Goal, Program, TraitId, Ty, Substitution, GoalData, DomainGoal, WhereClause, TraitRef,
    TraitDatum, AssociatedTyId,
};
use chalk_solve::{RustIrDatabase, solve::SLG, Solver, Solution};
use chalk_recursive::RecursiveSolver;
use rayon::prelude::*;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

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
pub struct MethodSig {
    pub params: Vec<Type>,
    pub ret: Type,
}

#[derive(Clone)]
pub struct Resolver {
    direct_impls: HashMap<(String, Type), HashMap<String, MethodSig>>,
    chalk_db: Arc<ChalkDatabase>,
    cache: Arc<RwLock<HashMap<(Type, String), Option<MethodSig>>>>,
    type_env: HashMap<String, Type>,
    borrow_checker: BorrowChecker,
}

struct ChalkDatabase {
    program: Program,
}

impl Resolver {
    pub fn new() -> Self {
        let program = build_program();
        let db = Arc::new(ChalkDatabase { program });
        let cache = Arc::new(RwLock::new(HashMap::new()));

        let mut r = Self {
            direct_impls: HashMap::new(),
            chalk_db: db,
            cache,
            type_env: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
        };

        // Fast-path i32 Addable
        let mut impls = HashMap::new();
        impls.insert("add".to_string(), MethodSig {
            params: vec![Type::I32],
            ret: Type::I32,
        });
        r.direct_impls.insert(("Addable".to_string(), Type::I32), impls);

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
            "i32" => Type::I32,
            "f32" => Type::F32,
            "bool" => Type::Bool,
            _ => Type::Named(s.to_string()),
        }
    }

    pub fn infer_type(&mut self, node: &AstNode) -> Type {
        match node {
            AstNode::Lit(n) if *n >= -2147483648 && *n <= 2147483647 => Type::I32,
            AstNode::Var(v) => self.type_env.get(v).cloned().unwrap_or(Type::Unknown),
            AstNode::Call { receiver, method, args, .. } => {
                let recv_ty = self.infer_type(receiver);

                // Fast path
                if let Some(impls) = self.direct_impls.get(&("Addable".to_string(), recv_ty.clone())) {
                    if let Some(sig) = impls.get(method) {
                        return sig.ret.clone();
                    }
                }

                // Chalk++ lookup
                if let Some(sig) = self.chalk_lookup(&recv_ty, method) {
                    return sig.ret.clone();
                }

                // Fallback
                if method == "add" && args.len() == 1 {
                    let arg_ty = self.infer_type(&args[0]);
                    if recv_ty == arg_ty {
                        return recv_ty;
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

    fn chalk_lookup(&self, ty: &Type, method: &str) -> Option<MethodSig> {
        let key = (ty.clone(), method.to_string());
        {
            let cache = self.cache.read().unwrap();
            if let Some(cached) = cache.get(&key) {
                return cached.clone();
            }
        }

        let db = self.chalk_db.clone();
        let cache = self.cache.clone();

        let result = rayon::spawn(move || {
            let solver = RecursiveSolver::new(&*db, 1000, None);
            let goal = Goal::new(&db.program.interner, GoalData::DomainGoal(DomainGoal::Holds(
                WhereClause::Implemented(TraitRef {
                    trait_id: TraitId::from_usize(0),
                    substitution: Substitution::empty(&db.program.interner),
                })
            )));
            match solver.solve(&goal) {
                Some(Solution::Unique(_)) => Some(MethodSig { params: vec![], ret: Type::I32 }),
                _ => None,
            }
        }).join();

        cache.write().unwrap().insert(key, result.clone());
        result
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

                    // CTFE
                    if chain.len() >= 2 {
                        let mut values = Vec::new();
                        let mut all_const = true;

                        for &id in &chain {
                            if let Some(expr) = mir.exprs.get(&id) {
                                match expr {
                                    MirExpr::Lit(v) => values.push(*v),
                                    MirExpr::ConstEval(v) => values.push(*v),
                                    _ => { all_const = false; break; }
                                }
                            } else {
                                all_const = false;
                                break;
                            }
                        }

                        if all_const {
                            let folded = if op == SemiringOp::Add {
                                values.iter().sum::<i64>()
                            } else {
                                values.iter().product::<i64>()
                            };

                            mir.stmts[i] = MirStmt::Assign {
                                lhs: current,
                                rhs: MirExpr::ConstEval(folded),
                            };
                            mir.stmts.drain(i + 1..j);
                            changed = true;
                            i += 1;
                            continue;
                        }
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

fn build_program() -> Program {
    let interner = Interner::new();
    let mut program = Program::new(&interner);

    let addable_id = TraitId::from_usize(0);
    program.trait_datum.insert(addable_id, TraitDatum {
        id: addable_id,
        binders: vec![],
        flags: Default::default(),
        associated_ty_ids: vec![],
        where_clauses: vec![],
    });

    program
}

impl RustIrDatabase for ChalkDatabase {
    fn interner(&self) -> &Interner {
        &self.program.interner
    }

    fn program(&self) -> &Program {
        &self.program
    }
}
