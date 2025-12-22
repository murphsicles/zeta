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
    /// Creates a new MIR generator.
    pub fn new() -> Self {
        Self {
            next_id: 0,
            stmts: vec![],
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
        }
    }

    /// Lowers an AST node to MIR using the generator.
    pub fn lower_to_mir(&mut self, ast: &AstNode) -> Mir {
        // Implement full lowering: traverse AST, assign IDs, build stmts/exprs
        // For buildability, example logic
        let name = if let AstNode::FuncDef { name, .. } = ast { Some(name.clone()) } else { None };
        let param_indices = if let AstNode::FuncDef { params, .. } = ast {
            params.iter().enumerate().map(|(i, (n, _))| (n.clone(), i as u32)).collect()
        } else {
            vec![]
        };
        // Add stmts, exprs, etc.
        Mir {
            name,
            param_indices,
            stmts: std::mem::take(&mut self.stmts),
            exprs: std::mem::take(&mut self.exprs),
            ctfe_consts: std::mem::take(&mut self.ctfe_consts),
        }
    }

    // Helper to generate next ID
    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    // Example: add expr
    fn add_expr(&mut self, expr: MirExpr) -> u32 {
        let id = self.next_id();
        self.exprs.insert(id, expr);
        id
    }

    // Example: add stmt
    fn add_stmt(&mut self, stmt: MirStmt) {
        self.stmts.push(stmt);
    }
}
