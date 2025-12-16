// src/mir.rs
//! Mid-level IR for Zeta, bridging AST to LLVM.
//! Supports statements, expressions, and semiring ops for algebraic optimization.
//! Added: ParamInit - store caller args to local allocas at fn entry.
//! Added: Affine moves - Consume stmt after calls for by-value args (semantic marker).
//! Updated Dec 9, 2025: Added StringLit to MirExpr for unified string lowering.
//! Updated Dec 13, 2025: Added FString lowering to concat calls; BinaryOp to method calls.
//! Updated Dec 16, 2025: Added If stmt for control flow (used in ? lowering); lowering for TryProp (with branch for error prop), DictLit (map_new + inserts), Subscript (map_get), Return; updated gen_mir for implicit wrap if error-prop fn.

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
    If {
        cond: u32,
        then: Vec<MirStmt>,
        else_: Vec<MirStmt>,
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

        if let AstNode::FuncDef { params, body, ret, .. } = ast {
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
                let ret_val = self.gen_expr(expr, &mut exprs, &mut stmts);
                let ret_id = self.materialize(ret_val, &mut exprs, &mut stmts);
                stmts.push(MirStmt::Return { val: ret_id });
            } else if !body.is_empty() && !matches!(body.last(), Some(AstNode::Return(_))) {
                // General implicit return for last expr
                if let Some(last) = body.last() {
                    let last_expr = self.gen_expr(last, &mut exprs, &mut stmts);
                    let last_id = self.materialize(last_expr, &mut exprs, &mut stmts);
                    // If fn ret is Result, wrap in make_ok
                    let is_error_prop = ret == "Result<i64,i64>"; // Stub: assume based on ret string
                    if is_error_prop {
                        let wrap_id = self.next_id();
                        stmts.push(MirStmt::Call { func: "result_make_ok".to_string(), args: vec![last_id], dest: wrap_id });
                        stmts.push(MirStmt::Return { val: wrap_id });
                    } else {
                        stmts.push(MirStmt::Return { val: last_id });
                    }
                }
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
            AstNode::Spawn { func: _, args } => {
                let mut arg_ids = vec![];
                for arg in args.iter() {
                    let e = self.gen_expr(arg, exprs, out);
                    let arg_id = self.materialize(e, exprs, out);
                    arg_ids.push(arg_id);
                }
                out.push(MirStmt::VoidCall { func: "spawn".to_string(), args: arg_ids });
            }
            AstNode::Assign(lhs, rhs) => {
                let rhs_expr = self.gen_expr(rhs.as_ref(), exprs, out);
                let rhs_id = self.materialize(rhs_expr, exprs, out);
                match **lhs {
                    AstNode::Var(ref v) => {
                        let lhs_id = self.lookup_or_alloc(v);
                        out.push(MirStmt::Assign { lhs: lhs_id, rhs: MirExpr::Var(rhs_id) });
                    }
                    AstNode::Subscript { ref base, ref index } => {
                        let base_expr = self.gen_expr(base.as_ref(), exprs, out);
                        let base_id = self.materialize(base_expr, exprs, out);
                        let index_expr = self.gen_expr(index.as_ref(), exprs, out);
                        let index_id = self.materialize(index_expr, exprs, out);
                        out.push(MirStmt::VoidCall { func: "map_insert".to_string(), args: vec![base_id, index_id, rhs_id] });
                    }
                    _ => {},
                }
            }
            AstNode::Return(inner) => {
                let ret_expr = self.gen_expr(inner.as_ref(), exprs, out);
                let ret_id = self.materialize(ret_expr, exprs, out);
                out.push(MirStmt::Return { val: ret_id });
            }
            _ => {
                // For expr stmts, materialize but void
                let e = self.gen_expr(node, exprs, out);
                let _ = self.materialize(e, exprs, out);
            }
        }
    }

    fn gen_expr(&mut self, node: &AstNode, exprs: &mut HashMap<u32, MirExpr>, out: &mut Vec<MirStmt>) -> MirExpr {
        match node {
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::StringLit(s) => MirExpr::StringLit(s.clone()),
            AstNode::FString(parts) => {
                let mut ids = vec![];
                for part in parts {
                    let e = self.gen_expr(part.as_ref(), exprs, out);
                    let id = self.materialize_inner(&e, exprs);
                    ids.push(id);
                }
                MirExpr::FString(ids)
            }
            AstNode::Var(v) => MirExpr::Var(self.lookup_or_alloc(v)),
            AstNode::TimingOwned { inner, .. } => {
                let inner_expr = self.gen_expr(inner.as_ref(), exprs, out);
                let inner_id = self.materialize_inner(&inner_expr, exprs);
                MirExpr::TimingOwned(inner_id)
            }
            AstNode::Call { receiver, method, args, .. } => {
                let receiver_id = if let Some(r) = receiver.as_ref() {
                    let e = self.gen_expr(r, exprs, out);
                    Some(self.materialize(e, exprs, out))
                } else {
                    None
                };
                let mut arg_ids = args
                    .iter()
                    .map(|a| {
                        let e = self.gen_expr(a, exprs, out);
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
                    out.push(MirStmt::Consume { id: **arg_id });
                }
                MirExpr::Var(dest)
            }
            AstNode::TryProp { expr } => {
                let inner_expr = self.gen_expr(expr.as_ref(), exprs, out);
                let inner_id = self.materialize(inner_expr, exprs, out);
                let cond_dest = self.next_id();
                out.push(MirStmt::Call { func: "result_is_ok".to_string(), args: vec![inner_id], dest: cond_dest });
                let data_id = self.next_id();
                let ok_dest = self.next_id();
                let then = vec![
                    MirStmt::Call { func: "result_get_data".to_string(), args: vec![inner_id], dest: data_id },
                    MirStmt::VoidCall { func: "result_free".to_string(), args: vec![inner_id] },
                    MirStmt::Assign { lhs: ok_dest, rhs: MirExpr::Var(data_id) },
                ];
                let else_ = vec![
                    MirStmt::Return { val: inner_id },
                ];
                out.push(MirStmt::If { cond: cond_dest, then, else_ });
                MirExpr::Var(ok_dest)
            }
            AstNode::DictLit { entries } => {
                let dest = self.next_id();
                out.push(MirStmt::Call { func: "map_new".to_string(), args: vec![], dest });
                for (k, v) in entries {
                    let k_expr = self.gen_expr(k.as_ref(), exprs, out);
                    let k_id = self.materialize(k_expr, exprs, out);
                    let v_expr = self.gen_expr(v.as_ref(), exprs, out);
                    let v_id = self.materialize(v_expr, exprs, out);
                    out.push(MirStmt::VoidCall { func: "map_insert".to_string(), args: vec![dest, k_id, v_id] });
                }
                MirExpr::Var(dest)
            }
            AstNode::Subscript { base, index } => {
                let base_expr = self.gen_expr(base.as_ref(), exprs, out);
                let base_id = self.materialize(base_expr, exprs, out);
                let index_expr = self.gen_expr(index.as_ref(), exprs, out);
                let index_id = self.materialize(index_expr, exprs, out);
                let dest = self.next_id();
                out.push(MirStmt::Call { func: "map_get".to_string(), args: vec![base_id, index_id], dest });
                MirExpr::Var(dest)
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
