// src/mir.rs
//! Mid-level IR for Zeta, bridging AST to LLVM.
//! Supports statements, expressions, and semiring ops for algebraic optimization.

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
            // Alloc param locals (no init here; caller passes args)
            for (pname, _) in params {
                self.alloc_local(pname);
            }
            for stmt in body {
                match stmt {
                    AstNode::Defer(boxed) => {
                        let unboxed = *boxed;
                        if let AstNode::Call {
                            receiver: None,
                            ref method,
                            ref args,
                            ..
                        } = unboxed
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
                        for arg in args {
                            let e = self.gen_expr(&arg, &mut exprs);
                            arg_ids.push(self.materialize(e, &mut exprs, &mut stmts));
                        }
                        let dest = self.next_id();
                        stmts.push(MirStmt::Call {
                            func: format!("actor_spawn_{}", func),
                            args: arg_ids,
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
                    args: arg_ids,
                    dest,
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
