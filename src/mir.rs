// src/mir.rs
//! Mid-level IR for Zeta, bridging AST to LLVM.
//! Supports statements, expressions, and semiring ops for algebraic optimization.
//! Added: ParamInit - store caller args to local allocas at fn entry.
//! Added: Affine moves - Consume stmt after calls for by-value args (semantic marker).
//! Added: CTFE pass - constant folding on literals and semiring ops before codegen.

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
    ParamInit {  // Initialize param local from arg index
        param_id: u32,
        arg_index: usize,
    },
    Consume {  // New: Mark local as consumed post-move (affine semantic)
        id: u32,
    },
}

#[derive(Debug, Clone)]
pub enum MirExpr {
    Var(u32),
    Lit(i64),
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
    param_indices: Vec<(String, usize)>,  // (name, arg position)
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
                match stmt {
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
                            let e = self.gen_expr(arg, &mut exprs);
                            let arg_id = self.materialize(e, &mut exprs, &mut stmts);
                            arg_ids.push(arg_id);
                            // Consume args post-spawn (affine move to actor)
                            stmts.push(MirStmt::Consume { id: arg_id });
                        }
                        let dest = self.next_id();
                        stmts.push(MirStmt::Call {
                            func: format!("actor_spawn_{}", func),
                            args: arg_ids.clone(),
                            dest,
                        });
                    }
                    _ => self.gen_stmt(stmt, &mut stmts, &mut exprs),
                }
            }

            // Insert defers before return in reverse order
            for info in self.defers.iter().rev() {
                stmts.push(MirStmt::VoidCall {
                    func: info.func.clone(),
                    args: info.args.clone(),
                });
            }

            if let Some(AstNode::Assign(_, expr)) = body.last() {
                let ret_val = self.gen_expr(expr, &mut exprs);
                let ret_id = self.materialize(ret_val, &mut exprs, &mut stmts);
                stmts.push(MirStmt::Return { val: ret_id });
            }
        }

        let mut mir = Mir {
            stmts,
            locals: self.locals.clone(),
            exprs,
            name,
        };

        // Apply CTFE optimization
        mir.optimize_ctfe();

        mir
    }

    /// CTFE optimization pass: Fold constant expressions in MIR.
    fn optimize_ctfe(&mut self, mir: &mut Mir) {
        let mut i = 0;
        while i < mir.stmts.len() {
            if let MirStmt::Assign { lhs, rhs } = &mut mir.stmts[i] {
                if let Some(folded) = self.eval_expr(rhs, &mir.exprs) {
                    *rhs = folded;
                    // If lhs is used only here, could propagate, but simple: keep assign to Lit
                }
            } else if let MirStmt::Call { func, args, dest } = &mut mir.stmts[i] {
                // If call to semiring op with all const args, fold
                if func == "add" || func == "mul" {
                    let op = if func == "add" { SemiringOp::Add } else { SemiringOp::Mul };
                    let all_const = args.iter().all(|&id| matches!(mir.exprs.get(&id), Some(MirExpr::Lit(_))));
                    if all_const {
                        let values: Vec<i64> = args.iter().map(|&id| if let MirExpr::Lit(n) = mir.exprs[&id] { *n } else { 0 }).collect();
                        let folded = match op {
                            SemiringOp::Add => values.iter().sum(),
                            SemiringOp::Mul => values.iter().product(),
                        };
                        mir.exprs.insert(*dest, MirExpr::ConstEval(folded));
                        // Replace stmt with assign from ConstEval
                        mir.stmts[i] = MirStmt::Assign { lhs: *dest, rhs: MirExpr::ConstEval(folded) };
                    }
                }
            }
            i += 1;
        }
    }

    fn eval_expr(&self, expr: &MirExpr, exprs: &HashMap<u32, MirExpr>) -> Option<MirExpr> {
        match expr {
            MirExpr::Var(id) => if let Some(e) = exprs.get(id) { self.eval_expr(e, exprs) } else { None },
            MirExpr::Lit(_) | MirExpr::ConstEval(_) => Some(expr.clone()),
            MirExpr::TimingOwned(id) => if let Some(inner) = exprs.get(id) { self.eval_expr(inner, exprs) } else { None },
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
                let rhs = self.gen_expr(expr, exprs);
                out.push(MirStmt::Assign { lhs, rhs });
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
                for arg_id in arg_ids.iter().filter(|id| matches!(exprs.get(id), Some(MirExpr::Var(_)))) {
                    out.push(MirStmt::Consume { id: *arg_id });
                }
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
        let mut mir_gen = MirGen::new(); // Reuse gen for eval
        mir_gen.optimize_ctfe(self);
    }
}
