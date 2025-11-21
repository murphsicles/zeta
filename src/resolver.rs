// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use chalk_solve::{
    RustIrDatabase, Solution, logic::NoSolution, solve::Guidance,
    Clause, Program, Goal, Interner, rust_ir::{self, GoalData},
};
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

#[derive(Clone)]
pub struct Resolver {
    // Fast path: direct impls
    direct_impls: HashMap<(String, Type), HashMap<String, MethodSig>>,
    // Chalk database
    chalk_db: Arc<ChalkDatabase>,
    // Incremental cache
    solution_cache: Arc<RwLock<HashMap<(Type, String), Option<MethodSig>>>>,
    // Borrow checker
    borrow_checker: BorrowChecker,
}

struct ChalkDatabase {
    program: Program<rust_ir::InternerImpl>,
}

impl Resolver {
    pub fn new() -> Self {
        let program = build_chalk_program();
        let chalk_db = Arc::new(ChalkDatabase { program });

        let mut r = Self {
            direct_impls: HashMap::new(),
            chalk_db,
            solution_cache: Arc::new(RwLock::new(HashMap::new())),
            borrow_checker: BorrowChecker::new(),
        };

        // Register built-in Addable<i32>
        let mut impls = HashMap::new();
        impls.insert("add".to_string(), MethodSig {
            params: vec![Type::I32],
            ret: Type::I32,
        });
        r.direct_impls.insert(("Addable".to_string(), Type::I32), impls);

        r
    }

    pub fn register(&mut self, ast: AstNode) {
        // Fast path registration
        match ast {
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
                self.direct_impls.insert((concept, ty), methods);
            }
            AstNode::FuncDef { name, .. } => {
                // Will be resolved lazily
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
            AstNode::Var(v) => Type::Unknown, // will be filled later
            AstNode::Call { receiver, method, args, .. } => {
                let recv_ty = self.infer_type(receiver);

                // 1. Fast direct lookup
                if let Some(impls) = self.direct_impls.get(&(method.clone(), recv_ty.clone())) {
                    if let Some(sig) = impls.get(method) {
                        return sig.ret.clone();
                    }
                }

                // 2. Parallel Chalk++ query
                if let Some(sig) = self.chalk_lookup_parallel(&recv_ty, method) {
                    return sig.ret.clone();
                }

                // 3. Fallback
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
                ty
            }
            _ => Type::Unknown,
        }
    }

    fn chalk_lookup_parallel(&self, ty: &Type, method: &str) -> Option<MethodSig> {
        let cache_key = (ty.clone(), method.to_string());
        {
            let cache = self.solution_cache.read().unwrap();
            if let Some(cached) = cache.get(&cache_key) {
                return cached.clone();
            }
        }

        let goal = build_goal(ty, method);
        let db = self.chalk_db.clone();
        let cache = self.solution_cache.clone();

        let result: Option<MethodSig> = rayon::spawn(move || {
            let solver = chalk_solve::Solver::new(&*db);
            match solver.solve(&goal) {
                Some(Solution::Unique(subst)) => {
                    let ret_ty = extract_return_type(&subst);
                    Some(MethodSig {
                        params: vec![],
                        ret: ret_ty,
                    })
                }
                _ => None,
            }
        }).join();

        let mut cache = cache.write().unwrap();
        cache.insert(cache_key, result.clone());
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
        let mut mir_gen = MirGen::new();
        mir_gen.gen_mir(ast)
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

// Chalk program construction (simplified)
fn build_chalk_program() -> Program<rust_ir::InternerImpl> {
    let interner = rust_ir::InternerImpl::default();
    let mut program = Program::new(&interner);

    // Addable<T> { add(T) -> T }
    let addable = rust_ir::TraitId::from_usize(0);
    let add_method = rust_ir::AssociatedTyId::from_usize(0);

    program.trait_datum.insert(addable, rust_ir::TraitDatum {
        id: addable,
        binders: vec![],
        flags: rust_ir::TraitFlags::default(),
        associated_ty_ids: vec![add_method],
        where_clause: vec![],
    });

    program
}

fn build_goal(ty: &Type, method: &str) -> Goal<rust_ir::InternerImpl> {
    // Simplified: create goal "Implemented(Addable<ty>)"
    Goal::new(GoalData::DomainGoal(rust_ir::DomainGoal::Holds(
        rust_ir::WhereClause::Implemented(rust_ir::TraitRef {
            trait_id: rust_ir::TraitId::from_usize(0),
            substitution: vec![],
        })
    )))
}

fn extract_return_type(_subst: &chalk_solve::Substitution<rust_ir::InternerImpl>) -> Type {
    Type::I32 // placeholder
}

impl RustIrDatabase<rust_ir::InternerImpl> for ChalkDatabase {
    fn program(&self) -> &Program<rust_ir::InternerImpl> {
        &self.program
    }
}
