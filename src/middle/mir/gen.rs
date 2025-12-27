// src/middle/mir/gen.rs
use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt};
use std::collections::HashMap;

pub struct MirGen {
    next_id: u32,
    stmts: Vec<MirStmt>,
    exprs: HashMap<u32, MirExpr>,
    ctfe_consts: HashMap<u32, i64>,
    defers: Vec<u32>, // Track defer cleanup ids for RAII
}

impl MirGen {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            stmts: vec![],
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
            defers: vec![],
        }
    }

    pub fn lower_to_mir(&mut self, ast: &AstNode) -> Mir {
        self.lower_ast(ast);
        Mir {
            name: if let AstNode::FuncDef { name, .. } = ast {
                Some(name.clone())
            } else {
                None
            },
            param_indices: if let AstNode::FuncDef { params, .. } = ast {
                params
                    .iter()
                    .enumerate()
                    .map(|(i, (n, _))| (n.clone(), i as u32))
                    .collect()
            } else {
                vec![]
            },
            stmts: std::mem::take(&mut self.stmts),
            exprs: std::mem::take(&mut self.exprs),
            ctfe_consts: std::mem::take(&mut self.ctfe_consts),
        }
    }

    fn lower_ast(&mut self, ast: &AstNode) {
        match ast {
            AstNode::Assign(lhs, rhs) => {
                let rhs_id = self.lower_expr(rhs);
                let lhs_id = self.lower_expr(lhs);
                self.stmts.push(MirStmt::Assign {
                    lhs: lhs_id,
                    rhs: rhs_id,
                });
            }
            AstNode::Return(inner) => {
                let val = self.lower_expr(inner);
                self.stmts.push(MirStmt::Return { val });
            }
            AstNode::BinaryOp { op, left, right } => {
                let left_id = self.lower_expr(left);
                let right_id = self.lower_expr(right);
                let dest = self.next_id();
                self.stmts.push(MirStmt::Call {
                    func: op.clone(),
                    args: vec![left_id, right_id],
                    dest,
                    type_args: vec![],
                });
            }
            AstNode::Defer(inner) => {
                // Lower the deferred block
                let start_id = self.next_id;
                self.lower_ast(inner);
                let cleanup_id = self.next_id - 1; // Last expr from inner as cleanup marker
                self.defers.push(cleanup_id);
            }
            AstNode::FuncDef { body, .. } => {
                let mut local_defers = vec![];
                for stmt in body {
                    if let AstNode::Defer(_) = stmt {
                        local_defers.push(stmt.clone());
                    }
                    self.lower_ast(stmt);
                }
                // Emit defer cleanups in reverse order (RAII LIFO)
                for defer in local_defers.iter().rev() {
                    let cleanup_id = self.defers.pop().unwrap_or(0);
                    self.stmts.push(MirStmt::VoidCall {
                        func: "__defer_cleanup".to_string(),
                        args: vec![cleanup_id],
                    });
                }
            }
            AstNode::If { cond, then, else_ } => {
                let cond_id = self.lower_expr(cond);
                self.stmts.push(MirStmt::If {
                    cond: cond_id,
                    then: then.iter().map(|s| {
                        let mut gen = MirGen::new();
                        gen.lower_ast(s);
                        gen.stmts
                    }).flatten().collect(),
                    else_: else_.iter().map(|s| {
                        let mut gen = MirGen::new();
                        gen.lower_ast(s);
                        gen.stmts
                    }).flatten().collect(),
                });
            }
            _ => {}
        }
    }

    fn lower_expr(&mut self, expr: &AstNode) -> u32 {
        let id = self.next_id();
        let mir_expr = match expr {
            AstNode::Var(_name) => MirExpr::Var(id),
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::StringLit(s) => MirExpr::StringLit(s.clone()),
            AstNode::FString(parts) => {
                let ids = parts
                    .iter()
                    .map(|p| self.lower_expr(p))
                    .collect();
                MirExpr::FString(ids)
            }
            AstNode::TimingOwned { inner, .. } => MirExpr::TimingOwned(self.lower_expr(inner)),
            _ => MirExpr::Lit(0),
        };
        self.exprs.insert(id, mir_expr);
        id
    }

    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

impl Default for MirGen {
    fn default() -> Self {
        Self::new()
    }
}
