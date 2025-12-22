// src/middle/mir/gen.rs
//! MIR generation utilities for Zeta.
use crate::ast::AstNode;
use crate::mir::{Mir, MirStmt, MirExpr, SemiringOp};
use std::collections::HashMap;

pub struct MirGen {
    next_id: u32,
    stmts: Vec<MirStmt>,
    exprs: HashMap<u32, MirExpr>,
    ctfe_consts: HashMap<u32, i64>,
}

impl MirGen {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            stmts: vec![],
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
        }
    }

    pub fn lower_to_mir(&mut self, ast: &AstNode) -> Mir {
        self.lower_ast(ast);
        Mir {
            name: if let AstNode::FuncDef { name, .. } = ast { Some(name.clone()) } else { None },
            param_indices: if let AstNode::FuncDef { params, .. } = ast {
                params.iter().enumerate().map(|(i, (n, _))| (n.clone(), i as u32)).collect()
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
                self.stmts.push(MirStmt::Assign { lhs: lhs_id, rhs: rhs_id });
            }
            AstNode::Return(inner) => {
                let val = self.lower_expr(inner);
                self.stmts.push(MirStmt::Return { val });
            }
            AstNode::BinaryOp { op, left, right } => {
                let left_id = self.lower_expr(left);
                let right_id = self.lower_expr(right);
                let dest = self.next_id();
                self.stmts.push(MirStmt::Call { func: op.clone(), args: vec![left_id, right_id], dest, type_args: vec![] });
            }
            // Add cases for other nodes, recursively for bodies
            AstNode::FuncDef { body, .. } => {
                for stmt in body {
                    self.lower_ast(stmt);
                }
            }
            // etc.
            _ => {},
        }
    }

    fn lower_expr(&mut self, expr: &AstNode) -> u32 {
        let id = self.next_id();
        let mir_expr = match expr {
            AstNode::Var(name) => MirExpr::Var(id), // Would map name to ID
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::StringLit(s) => MirExpr::StringLit(s.clone()),
            AstNode::FString(parts) => {
                let ids = parts.iter().map(|p| self.lower_ast(p); self.next_id() - 1).collect();
                MirExpr::FString(ids)
            }
            AstNode::TimingOwned { inner, .. } => MirExpr::TimingOwned(self.lower_expr(inner)),
            // etc.
            _ => MirExpr::Lit(0), // Stub for others
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
