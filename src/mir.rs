// src/mir.rs
//! Mid-level IR for Zeta, bridging AST to LLVM.
//! Supports statements, expressions, and semiring ops for algebraic optimization.
//! Added: ParamInit - store caller args to local allocas at fn entry.
//! Added: Affine moves - Consume stmt after calls for by-value args (semantic marker).
//! Updated Dec 9, 2025: Added StringLit to MirExpr for unified string lowering.
//! Updated Dec 13, 2025: Added FString lowering to concat calls; BinaryOp to method calls.

use crate::ast::AstNode;
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
}

#[derive(Debug, Clone)]
pub enum MirExpr {
    Var(u32),
    Lit(i64),
    StringLit(String), // NEW: unified string literal
    FString(Vec<u32>), // Lowered parts as IDs
    ConstEval(i64),
    TimingOwned(u32), // Wraps inner expr ID for constant-time
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
                self.gen_stmt_full(stmt, &mut stmts, &mut exprs);
            }

            // Insert defers before return in reverse order
            for info in self.defers.iter().rev() {
                stmts.push(MirStmt::VoidCall {
                    func: info.func.clone(),
                    args: info.args.clone(),
                });
            }

            // Assume last stmt is return if Assign
            if let Some(last) = body.last()
                && let AstNode::Assign(_, expr) = last
            {
                let ret_val = self.gen_expr(expr, &mut exprs);
                let ret_id = self.materialize(ret_val, &mut exprs, &mut stmts);
                stmts.push(MirStmt::Return { val: ret_id });
            }
        }
        Mir {
            stmts,
            locals: self.locals.clone(),
            exprs,
            name,
        }
    }

    fn gen_stmt_full(
        &mut self,
        node: &AstNode,
        out: &mut Vec<MirStmt>,
        exprs: &mut HashMap<u32, MirExpr>,
    ) {
        match node {
            AstNode::Defer(boxed) => {
                if let AstNode::Call {
                    receiver: None,
                    ref method,
                    ref args,
                    ..
                } = **boxed
                {
                    let mut arg_ids = vec![];
                    for arg in args.iter() {
                        if let AstNode::Var(ref v) = *arg {
                            let id = self.lookup_or_alloc(v);
                            arg_ids.push(id);
                        }
                    }
                    self.defers.push(DeferInfo {
                        func: method.clone(),
                        args: arg_ids,
                    });
                }
            }
            AstNode::Spawn { func, args } => {
                let mut arg_ids = vec![];
                for arg in args.iter() {
                    let e = self.gen_expr(arg, exprs);
                    let arg_id = self.materialize(e, exprs, out);
                    arg_ids.push(arg_id);
                }
                out.push(MirStmt::VoidCall {
                    func: format!("actor_spawn_{}", func),
                    args: arg_ids,
                });
            }
            AstNode::Assign(lhs, rhs) => {
                let rhs_expr = self.gen_expr(rhs, exprs);
                let rhs_id = self.materialize(rhs_expr, exprs, out);
                let lhs_id = self.lookup_or_alloc(lhs);
                out.push(MirStmt::Assign { lhs: lhs_id, rhs: MirExpr::Var(rhs_id) });
            }
            AstNode::BinaryOp { op, left, right } => {
                let left_expr = self.gen_expr(left, exprs);
                let left_id = self.materialize(left_expr, exprs, out);
                let right_expr = self.gen_expr(right, exprs);
                let right_id = self.materialize(right_expr, exprs, out);
                let dest = self.next_id();
                let func = if op == "+" { "str_concat".to_string() } else { op.clone() };
                out.push(MirStmt::Call {
                    func,
                    args: vec![left_id, right_id],
                    dest,
                });
            }
            AstNode::Call {
                receiver,
                method,
                args,
                ..
            } => {
                let receiver_id = receiver.as_ref().map(|r| {
                    let e = self.gen_expr(r, exprs);
                    self.materialize(e, exprs, out)
                });

                let mut arg_ids = args
                    .iter()
                    .map(|a| {
                        let e = self.gen_expr(a, exprs);
                        self.materialize(e, exprs, out)
                    })
                    .collect::<Vec<_>>();

                if let Some(rid) = receiver_id {
                    arg_ids.insert(0, rid);
                }

                let dest = self.next_id();
                out.push(MirStmt::Call {
                    func: method.clone(),
                    args: arg_ids.clone(),
                    dest,
                });

                // Affine: Consume by-value args post-call (assume all Var args moved)
                for arg_id in arg_ids
                    .iter()
                    .filter(|id| matches!(exprs.get(id), Some(MirExpr::Var(_))))
                {
                    out.push(MirStmt::Consume { id: *arg_id });
                }
            }
            _ => {}
        }
    }

    fn gen_expr(&mut self, node: &AstNode, exprs: &mut HashMap<u32, MirExpr>) -> MirExpr {
        match node {
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::StringLit(s) => MirExpr::StringLit(s.clone()),
            AstNode::FString(parts) => {
                let mut ids = vec![];
                for part in parts {
                    let e = self.gen_expr(part, exprs);
                    let id = self.materialize_inner(&e, exprs);
                    ids.push(id);
                }
                MirExpr::FString(ids)
            }
            AstNode::Var(v) => MirExpr::Var(self.lookup_or_alloc(v)),
            AstNode::TimingOwned { inner, .. } => {
                let inner_expr = self.gen_expr(inner.as_ref(), exprs);
                let inner_id = self.materialize_inner(&inner_expr, exprs);
                MirExpr::TimingOwned(inner_id)
            }
            AstNode::Call { .. } => {
                let id = self.next_id();
                MirExpr::Var(id)
            }
            _ => MirExpr::Lit(0),
        }
    }

    fn materialize_inner(&mut self, expr: &MirExpr, exprs: &mut HashMap<u32, MirExpr>) -> u32 {
        match expr {
            MirExpr::Var(id) => *id,
            _ => {
                let id = self.next_id();
                exprs.insert(id, expr.clone());
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
        id
    }

    fn lookup_or_alloc(&mut self, name: &str) -> u32 {
        let key = name.to_string();
        if !self.locals.contains_key(&key) {
            let id = self.next_id();
            self.locals.insert(key, id);
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
}
