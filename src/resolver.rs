// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
use crate::mir::{Mir, MirGen, MirStmt, MirExpr, SemiringOp};
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
    direct_impls: HashMap<(String, Type), HashMap<String, MethodSig>>,
    structural_impls: HashMap<String, HashMap<HashMap<String, Type>, MethodSig>>,
    chalk_db: Arc<ChalkDatabase>,
    solution_cache: Arc<RwLock<HashMap<(Type, String), Option<MethodSig>>>>,
    type_env: HashMap<String, Type>,
    borrow_checker: BorrowChecker,
    specializations: HashMap<(String, Vec<Type>), AstNode>,
}

struct ChalkDatabase {
    program: Program,
}

impl Resolver {
    pub fn new() -> Self {
        let program = build_chalk_program();
        let chalk_db = Arc::new(ChalkDatabase { program });
        let solution_cache = Arc::new(RwLock::new(HashMap::new()));

        let mut r = Self {
            direct_impls: HashMap::new(),
            structural_impls: HashMap::new(),
            chalk_db,
            solution_cache,
            type_env: HashMap::new(),
            borrow_checker: BorrowChecker::new(),
            specializations: HashMap::new(),
        };

        // Built-in Addable<i32>
        let mut impl_block = HashMap::new();
        impl_block.insert(
            "add".to_string(),
            MethodSig {
                params: vec![Type::I32],
                ret: Type::I32,
            },
        );
        r.direct_impls.insert(("Addable".to_string(), Type::I32), impl_block);

        // Structural impl for {x: i32, y: i32}.add() -> i32
        let mut fields = HashMap::new();
        fields.insert("x".to_string(), Type::I32);
        fields.insert("y".to_string(), Type::I32);
        let mut structural = HashMap::new();
        structural.insert(
            fields,
            MethodSig {
                params: vec![],
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
                // Add to Chalk program (simplified)
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
                self.direct_impls.insert((concept, ty), methods);
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

                // 1. Direct nominal lookup
                if let Some(impls) = self.direct_impls.get(&("Addable".to_string(), recv_ty.clone())) {
                    if let Some(sig) = impls.get(method) {
                        return sig.ret.clone();
                    }
                }

                // 2. Parallel Chalk++ structural lookup
                if let Some(sig) = self.chalk_lookup_parallel(&recv_ty, method) {
                    return sig.ret.clone();
                }

                // 3. Structural field match
                if let Type::Struct { fields, .. } = &recv_ty {
                    if let Some(structural) = self.structural_impls.get(method) {
                        if structural.contains_key(fields) {
                            return structural[fields].ret.clone();
                        }
                    }
                }

                // 4. Partial specialization fallback
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

    fn chalk_lookup_parallel(&self, ty: &Type, method: &str) -> Option<MethodSig> {
        let cache_key = (ty.clone(), method.to_string());
        {
            let cache = self.solution_cache.read().unwrap();
            if let Some(cached) = cache.get(&cache_key) {
                return cached.clone();
            }
        }

        let goal = self.build_goal(ty, method);
        let db = self.chalk_db.clone();
        let cache = self.solution_cache.clone();

        let result: Option<MethodSig> = rayon::spawn(move || {
            let solver = RecursiveSolver::new(&*db);
            match solver.solve(&goal) {
                Some(Solution::Unique(subst)) => {
                    let ret_ty = self.extract_return_type(&subst);
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

    fn build_goal(&self, ty: &Type, method: &str) -> Goal {
        // Simplified goal for structural match
        Goal::new(DomainGoal::Holds(WhereClause::Implemented(TraitRef {
            trait_id: TraitId::from_usize(0),
            substitution: vec![], // ty subst
        })))
    }

    fn extract_return_type(&self, _subst: &Substitution) -> Type {
        Type::I32 // placeholder for extracted ret
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
        let mut mir_gen = MirGen::new();
        mir_gen.gen_mir(ast)
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
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    // CTFE: fold if all literals/constants
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
                                if values.is_empty() { 1 } else { values.iter().product::<i64>() }
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

                    // Runtime SemiringFold for non-constants
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

fn build_chalk_program() -> Program {
    let interner = Interner::new();
    let mut program = Program::new(&interner);

    // Addable trait
    let addable_id = TraitId::new(&interner, ()).unwrap();
    program.trait_datum.insert(addable_id, chalk_ir::TraitDatum {
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
        chalk_ir::interner::Interner::new()
    }

    fn program(&self) -> &Program {
        &self.program
    }
}
