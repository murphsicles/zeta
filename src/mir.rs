// src/mir.rs
use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Mir {
    pub stmts: Vec<MirStmt>,
    pub locals: HashMap<String, u32>,
    pub ctfe_consts: HashMap<u32, i64>,
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
    },
    Borrow {
        var: u32,
    },
    Return {
        val: u32,
    },
    Defer {
        stmt: Box<MirStmt>,
    },
    SemiringOp {
        op: SemiringOp,
        lhs: u32,
        rhs: u32,
        res: u32,
    },
    Fusion {
        orig: Box<MirStmt>,
        fused: Box<MirStmt>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemiringOp {
    Add,
    Mul,
}

#[derive(Debug, Clone)]
pub enum MirExpr {
    Var(u32),
    Lit(i64),
    MethodCall {
        recv: u32,
        method: String,
        args: Vec<u32>,
    },
    ConstEval(i64),
}

pub struct MirGen {
    next_id: u32,
    locals: HashMap<String, u32>,
}

impl MirGen {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            locals: HashMap::new(),
        }
    }

    pub fn gen_mir(&mut self, ast: &AstNode) -> Mir {
        let mut stmts = vec![];
        let ctfe_consts = HashMap::new();
        match ast {
            AstNode::FuncDef { body, params, .. } => {
                for (pname, _) in params {
                    let id = self.next_id;
                    self.locals.insert(pname.clone(), id);
                    self.next_id += 1;
                }
                for node in body {
                    if let Some(stmt) = self.gen_stmt(node) {
                        stmts.push(stmt);
                    }
                }
                stmts.push(MirStmt::Return { val: 0 });
            }
            _ => {}
        }
        Mir {
            stmts,
            locals: self.locals.clone(),
            ctfe_consts,
        }
    }

    fn gen_stmt(&mut self, node: &AstNode) -> Option<MirStmt> {
        match node {
            AstNode::Assign(name, expr) => {
                let lhs = self.fresh_id();
                self.locals.insert(name.clone(), lhs);
                let rhs = self.gen_expr(expr.as_ref())?;
                Some(MirStmt::Assign { lhs, rhs })
            }
            AstNode::Call {
                receiver,
                method,
                args,
            } => {
                let _recv_id = *self.locals.get(receiver).unwrap_or(&0);
                let arg_ids: Vec<u32> = args.iter().map(|a| *self.locals.get(a).unwrap_or(&0)).collect();
                Some(MirStmt::Call {
                    func: format!("{receiver}.{method}"),
                    args: arg_ids,
                })
            }
            AstNode::Borrow(var) => {
                let vid = *self.locals.get(var).unwrap_or(&0);
                Some(MirStmt::Borrow { var: vid })
            }
            AstNode::Defer(inner) => self.gen_stmt(inner.as_ref()).map(|s| MirStmt::Defer { stmt: Box::new(s) }),
            _ => None,
        }
    }

    fn gen_expr(&mut self, node: &AstNode) -> Option<MirExpr> {
        match node {
            AstNode::Lit(n) => Some(MirExpr::Lit(*n)),
            AstNode::Var(v) => self.locals.get(v).map(|id| MirExpr::Var(*id)),
            AstNode::Call {
                receiver,
                method,
                args,
            } => {
                let recv_id = *self.locals.get(receiver).unwrap_or(&0);
                let arg_ids: Vec<u32> = args.iter().map(|a| *self.locals.get(a).unwrap_or(&0)).collect();
                Some(MirExpr::MethodCall {
                    recv: recv_id,
                    method: method.clone(),
                    args: arg_ids,
                })
            }
            _ => None,
        }
    }

    fn fresh_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}
