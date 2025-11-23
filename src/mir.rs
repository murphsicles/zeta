// src/mir.rs
use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemiringOp {
    Add,
    Mul,
}

#[derive(Debug, Clone)]
pub struct Mir {
    pub stmts: Vec<MirStmt>,
    pub locals: HashMap<String, u32>,
    pub exprs: HashMap<u32, MirExpr>,
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
        let mut exprs = HashMap::new();

        if let AstNode::FuncDef { body, .. } = ast {
            for stmt in body {
                self.gen_stmt(stmt, &mut stmts, &mut exprs);
            }

            if let Some(AstNode::Assign(name, expr)) = body.last() {
                if name == "_" {
                    let ret_val = self.gen_expr(expr, &mut exprs);
                    let ret_id = self.materialize(ret_val, &mut exprs, &mut stmts);
                    stmts.push(MirStmt::Return { val: ret_id });
                }
            }
        }

        Mir {
            stmts,
            locals: self.locals.clone(),
            exprs,
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

    fn gen_expr(&mut self, node: &AstNode, _exprs: &mut HashMap<u32, MirExpr>) -> MirExpr {
        match node {
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::Var(v) => MirExpr::Var(self.lookup_or_alloc(v)),
            AstNode::Call { .. } => {
                let id = self.next_id();
                MirExpr::Var(id)
            }
            _ => MirExpr::Lit(0),
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
        let id = self.next_id;
        *self.locals.entry(name.to_string()).or_insert(id)
    }

    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}
