// src/mir.rs
//! Mid-level IR for Zeta, bridging AST to LLVM.
//! Supports statements, expressions, and semiring ops for algebraic optimization.
//! Added: ParamInit - store caller args to local allocas at fn entry.
//! Added: Affine moves - Consume stmt after calls for by-value args (semantic marker).
//! Added: Basic Switch for match lowering (on i64).
//! Added: Dead code elim: remove unused locals/stmts.

use crate::ast::{AstNode, Pattern};
use rayon::prelude::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemiringOp {
    /// Addition (idempotent or not, per type).
    Add,
    /// Multiplication.
    Mul,
}

#[derive(Debug, Clone)]
pub struct Mir {
    pub stmts: Vec<MirStmt>,
    pub locals: HashMap<String, u32>,
    pub exprs: HashMap<u32, MirExpr>,
    pub name: Option<String>, // Fn name for codegen
}

#[derive(Debug, Clone)]
pub enum MirStmt {
    Assign {
        lhs: u32,
        rhs: MirExpr,
    },
    Call {
        func: String,
        args: Vec<u32>,
        dest: u32,
    },
    VoidCall {
        func: String,
        args: Vec<u32>,
    },
    Return {
        val: u32,
    },
    SemiringFold {
        op: SemiringOp,
        values: Vec<u32>,
        result: u32,
    },
    ParamInit {
        // Initialize param local from arg index
        param_id: u32,
        arg_index: usize,
    },
    Consume {
        // New: Mark local as consumed post-move (affine semantic)
        id: u32,
    },
    Switch {
        val: u32,
        arms: Vec<(i64, u32)>, // (lit, dest block id stub)
        default: u32,
    },
}

#[derive(Debug, Clone)]
pub enum MirExpr {
    Var(u32),
    Lit(i64),
    ConstEval(i64),
    TimingOwned(u32), // Wraps inner expr ID for constant-time
    GetField {
        base: u32,
        field_idx: usize,
    },
}

#[derive(Debug, Clone)]
struct DeferInfo {
    func: String,
    args: Vec<u32>,
}

pub struct MirGen {
    next_id: u32,
    locals: HashMap<String, u32>,
    defers: Vec<DeferInfo>,
    // Track param indices for init
    param_indices: Vec<(String, usize)>, // (name, arg position)
    #[allow(dead_code)]
    exprs: HashMap<u32, MirExpr>,
    // Used locals for dead code elim
    used_locals: HashMap<u32, bool>,
}

impl Default for MirGen {
    fn default() -> Self {
        Self::new()
    }
}

impl MirGen {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            locals: HashMap::new(),
            defers: vec![],
            param_indices: vec![],
            exprs: HashMap::new(),
            used_locals: HashMap::new(),
        }
    }

    pub fn gen_mir(&mut self, ast: &AstNode) -> Mir {
        let mut stmts = vec![];
        let mut exprs = HashMap::new();
        let name = if let AstNode::FuncDef { name, .. } = ast {
            Some(name.clone())
        } else {
            None
        };

        if let AstNode::FuncDef { params, body, .. } = ast {
            // Alloc param locals and track for init
            self.param_indices.clear();
            for (i, (pname, _)) in params.iter().enumerate() {
                self.alloc_local(pname);
                self.param_indices.push((pname.clone(), i));
            }

            // Add ParamInit stmts at entry
            for (pname, arg_idx) in &self.param_indices {
                if let Some(param_id) = self.locals.get(pname) {
                    stmts.push(MirStmt::ParamInit {
                        param_id: *param_id,
                        arg_index: *arg_idx,
                    });
                }
            }

            for stmt in body {
                self.gen_stmt(stmt, &mut stmts, &mut exprs);
            }

            // Insert defers before return in reverse order
            for info in self.defers.iter().rev() {
                stmts.push(MirStmt::VoidCall {
                    func: info.func.clone(),
                    args: info.args.clone(),
                });
            }

            // Stub return last
            if !stmts.is_empty() {
                if let MirStmt::Assign { lhs, .. } = stmts.last().unwrap() {
                    stmts.push(MirStmt::Return { val: *lhs });
                }
            }
        }

        // Dead code elim: remove unused
        let mut used = self.used_locals.clone();
        for stmt in &stmts {
            match stmt {
                MirStmt::Assign { lhs, .. } => {
                    used.insert(*lhs, true);
                }
                MirStmt::Call { dest, .. } => {
                    used.insert(*dest, true);
                }
                _ => {}
            }
        }
        let stmts = stmts.into_iter().filter(|stmt| {
            match stmt {
                MirStmt::Assign { lhs, .. } => *used.get(lhs).unwrap_or(&false),
                MirStmt::Call { dest, .. } => *used.get(dest).unwrap_or(&false),
                _ => true,
            }
        }).collect();

        Mir {
            stmts,
            locals: self.locals.clone(),
            exprs,
            name,
        }
    }

    fn gen_stmt(
        &mut self,
        node: &AstNode,
        out: &mut Vec<MirStmt>,
        exprs: &mut HashMap<u32, MirExpr>,
    ) {
        match node {
            AstNode::Assign(name, expr) => {
                let lhs = self.alloc_local(name);
                let rhs_id = self.materialize(self.gen_expr(expr, exprs), exprs, out);
                out.push(MirStmt::Assign { lhs, rhs: MirExpr::Var(rhs_id) });
            }
            AstNode::Call { receiver, method, args, .. } => {
                let mut call_args = vec![];
                if let Some(recv) = receiver {
                    let base_id = self.materialize(self.gen_expr(recv, exprs), exprs, out);
                    call_args.push(base_id);
                }
                for arg in args {
                    let arg_id = self.materialize(self.gen_expr(arg, exprs), exprs, out);
                    call_args.push(arg_id);
                    // Affine: consume after move
                    out.push(MirStmt::Consume { id: arg_id });
                }
                let dest = self.next_id();
                out.push(MirStmt::Call {
                    func: method.clone(),
                    args: call_args,
                    dest,
                });
            }
            AstNode::Match { expr, arms } => {
                let val_id = self.materialize(self.gen_expr(expr, exprs), exprs, out);
                let mut arm_dests = vec![];
                let default = self.next_id();
                for (pat, arm) in arms {
                    match pat {
                        Pattern::Lit(n) => {
                            let arm_id = self.materialize(self.gen_expr(arm, exprs), exprs, out);
                            arm_dests.push((*n, arm_id));
                        }
                        _ => {} // Other patterns stub to default
                    }
                }
                out.push(MirStmt::Switch {
                    val: val_id,
                    arms: arm_dests,
                    default,
                });
            }
            _ => {}
        }
    }

    fn gen_expr(&mut self, node: &AstNode, exprs: &mut HashMap<u32, MirExpr>) -> MirExpr {
        match node {
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::Var(v) => MirExpr::Var(self.lookup_or_alloc(v)),
            AstNode::TimingOwned { inner, .. } => {
                let inner_id = self.materialize_inner(inner, exprs);
                MirExpr::TimingOwned(inner_id)
            }
            AstNode::Call { .. } => {
                let id = self.next_id();
                MirExpr::Var(id)
            }
            _ => MirExpr::Lit(0),
        }
    }

    fn materialize_inner(&mut self, node: &AstNode, exprs: &mut HashMap<u32, MirExpr>) -> u32 {
        let expr = self.gen_expr(node, exprs);
        match expr {
            MirExpr::Var(id) => id,
            _ => {
                let id = self.next_id();
                exprs.insert(id, expr);
                id
            }
        }
    }

    fn materialize(
        &mut self,
        expr: MirExpr,
        exprs: &mut HashMap<u32, MirExpr>,
        out: &mut Vec<MirStmt>,
    ) -> u32 {
        match expr {
            MirExpr::Var(id) => id,
            MirExpr::TimingOwned(inner_id) => {
                let id = self.next_id();
                exprs.insert(id, MirExpr::TimingOwned(inner_id));
                id
            }
            _ => {
                let id = self.next_id();
                out.push(MirStmt::Assign {
                    lhs: id,
                    rhs: expr.clone(),
                });
                exprs.insert(id, expr);
                id
            }
        }
    }

    fn alloc_local(&mut self, name: &str) -> u32 {
        let id = self.next_id();
        self.locals.insert(name.to_string(), id);
        self.used_locals.insert(id, false);
        id
    }

    fn lookup_or_alloc(&mut self, name: &str) -> u32 {
        let key = name.to_string();
        if !self.locals.contains_key(&key) {
            let id = self.next_id();
            self.locals.insert(key, id);
            self.used_locals.insert(id, false);
            id
        } else {
            *self.locals.get(&key).unwrap()
        }
    }

    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

impl Mir {
    /// Apply CTFE pass to fold constants.
    pub fn optimize_ctfe(&mut self) {
        let mut i = 0;
        while i < self.stmts.len() {
            if let MirStmt::Call { func, args, dest } = &mut self.stmts[i] {
                // If call to semiring op with all const args, fold
                if func == "add" || func == "mul" {
                    let op = if func == "add" {
                        SemiringOp::Add
                    } else {
                        SemiringOp::Mul
                    };
                    let all_const = args
                        .iter()
                        .all(|&id| matches!(self.exprs.get(&id), Some(MirExpr::Lit(_))));
                    if all_const {
                        let values: Vec<i64> = args
                            .iter()
                            .map(|&id| {
                                if let MirExpr::Lit(n) = self.exprs[&id] {
                                    n
                                } else {
                                    0
                                }
                            })
                            .collect();
                        let folded = match op {
                            SemiringOp::Add => values.iter().sum(),
                            SemiringOp::Mul => values.iter().product(),
                        };
                        self.exprs.insert(*dest, MirExpr::ConstEval(folded));
                        // Replace stmt with assign from ConstEval
                        self.stmts[i] = MirStmt::Assign {
                            lhs: *dest,
                            rhs: MirExpr::ConstEval(folded),
                        };
                    }
                }
            }
            i += 1;
        }
    }

    /// Parallel dead code elim across MIRs.
    pub fn parallel_optimize(mirs: &mut [Mir]) {
        mirs.par_iter_mut().for_each(|mir| {
            // Dead code elim
            let mut used = HashMap::new();
            for stmt in &mir.stmts {
                match stmt {
                    MirStmt::Assign { lhs, .. } => {
                        used.insert(*lhs, true);
                    }
                    MirStmt::Call { dest, .. } => {
                        used.insert(*dest, true);
                    }
                    _ => {}
                }
            }
            mir.stmts.retain(|stmt| {
                match stmt {
                    MirStmt::Assign { lhs, .. } => *used.get(lhs).unwrap_or(&false),
                    MirStmt::Call { dest, .. } => *used.get(dest).unwrap_or(&false),
                    _ => true,
                }
            });
        });
    }
}
