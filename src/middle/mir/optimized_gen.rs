// src/middle/mir/optimized_gen.rs
//! Optimized MIR Generation with better data structures

use crate::frontend::ast::AstNode;
use crate::middle::mir::optimized_mir::{MirExpr, MirStmt, OptimizedMir};
use crate::middle::types::Type;

/// Optimized MIR generator using Vec-based storage
pub struct OptimizedMirGen {
    next_id: u32,
    stmts: Vec<MirStmt>,
    exprs: Vec<Option<MirExpr>>,
    ctfe_consts: Vec<Option<i64>>,
    type_map: Vec<Option<Type>>,
    name_to_id: Vec<Option<u32>>, // For small scopes, use Vec instead of HashMap
    names: Vec<String>,           // Store names for lookup
    capacity: usize,
}

impl Default for OptimizedMirGen {
    fn default() -> Self {
        Self::new()
    }
}

impl OptimizedMirGen {
    pub fn new() -> Self {
        let initial_capacity = 64; // Reasonable default for small functions
        Self {
            next_id: 1,
            stmts: Vec::new(),
            exprs: vec![None; initial_capacity],
            ctfe_consts: vec![None; initial_capacity],
            type_map: vec![None; initial_capacity],
            name_to_id: Vec::new(),
            names: Vec::new(),
            capacity: initial_capacity,
        }
    }

    /// Create with estimated capacity based on AST size
    pub fn with_capacity(ast: &AstNode) -> Self {
        let estimated_nodes = Self::estimate_ast_nodes(ast);
        let capacity = (estimated_nodes * 2).max(64); // Double for expressions

        Self {
            next_id: 1,
            stmts: Vec::with_capacity(estimated_nodes),
            exprs: vec![None; capacity],
            ctfe_consts: vec![None; capacity],
            type_map: vec![None; capacity],
            name_to_id: Vec::new(),
            names: Vec::new(),
            capacity,
        }
    }

    /// Estimate number of nodes in AST for pre-allocation
    fn estimate_ast_nodes(ast: &AstNode) -> usize {
        match ast {
            AstNode::FuncDef { body, .. } => {
                1 + body.iter().map(Self::estimate_ast_nodes).sum::<usize>()
            }
            AstNode::BinaryOp { left, right, .. } => {
                1 + Self::estimate_ast_nodes(left) + Self::estimate_ast_nodes(right)
            }
            AstNode::Call { args, .. } => {
                1 + args.iter().map(Self::estimate_ast_nodes).sum::<usize>()
            }
            AstNode::If {
                cond, then, else_, ..
            } => {
                1 + Self::estimate_ast_nodes(cond)
                    + then.iter().map(Self::estimate_ast_nodes).sum::<usize>()
                    + else_.iter().map(Self::estimate_ast_nodes).sum::<usize>()
            }
            AstNode::Let { expr, .. } => 1 + Self::estimate_ast_nodes(expr),
            AstNode::Return(inner) => 1 + Self::estimate_ast_nodes(inner),
            AstNode::ExprStmt { expr } => 1 + Self::estimate_ast_nodes(expr),
            _ => 1,
        }
    }

    fn ensure_capacity(&mut self, id: u32) {
        let needed = id as usize + 1;
        if needed > self.capacity {
            self.capacity = needed.next_power_of_two();
            self.exprs.resize(self.capacity, None);
            self.ctfe_consts.resize(self.capacity, None);
            self.type_map.resize(self.capacity, None);
        }
    }

    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.ensure_capacity(id);
        id
    }

    fn next_id_with_lit(&mut self, value: i64) -> u32 {
        let id = self.next_id();
        self.exprs[id as usize] = Some(MirExpr::Lit(value));
        self.ctfe_consts[id as usize] = Some(value);
        self.type_map[id as usize] = Some(Type::I64);
        id
    }

    fn get_name_id(&self, name: &str) -> Option<u32> {
        // Linear search is fine for small scopes (typically < 20 names)
        self.names
            .iter()
            .position(|n| n == name)
            .and_then(|idx| self.name_to_id[idx])
    }

    fn set_name_id(&mut self, name: String, id: u32) {
        self.names.push(name);
        self.name_to_id.push(Some(id));
    }

    pub fn lower_to_mir(&mut self, ast: &AstNode) -> OptimizedMir {
        // Clear state
        self.next_id = 1;
        self.stmts.clear();
        self.names.clear();
        self.name_to_id.clear();

        // Initialize arrays
        let initial_capacity = self.capacity;
        self.exprs = vec![None; initial_capacity];
        self.ctfe_consts = vec![None; initial_capacity];
        self.type_map = vec![None; initial_capacity];

        // Process function parameters
        if let AstNode::FuncDef { params, .. } = ast {
            for (i, (name, _)) in params.iter().enumerate() {
                let id = self.next_id();
                self.set_name_id(name.clone(), id);
                self.exprs[id as usize] = Some(MirExpr::Var(id));
                self.type_map[id as usize] = Some(Type::I64);
                self.stmts.push(MirStmt::ParamInit {
                    param_id: id,
                    arg_index: i as u32,
                });
            }
        }

        // Lower AST (simplified - full implementation would go here)
        self.lower_ast(ast);

        // Ensure return statement
        if self.stmts.is_empty() || !matches!(self.stmts.last(), Some(MirStmt::Return { .. })) {
            let ret_val = if let Some(last) = self.stmts.last() {
                match last {
                    MirStmt::Call { dest, .. } => *dest,
                    MirStmt::SemiringFold { result, .. } => *result,
                    MirStmt::Assign { lhs, .. } => *lhs,
                    MirStmt::DictGet { dest, .. } => *dest,
                    MirStmt::If {
                        dest: Some(dest_id),
                        ..
                    } => *dest_id,
                    _ => self.next_id_with_lit(0),
                }
            } else {
                self.next_id_with_lit(0)
            };
            self.stmts.push(MirStmt::Return { val: ret_val });
        }

        // Trim vectors to actual size
        let max_id = self.next_id - 1;
        let actual_size = max_id as usize + 1;

        if actual_size < self.exprs.len() {
            self.exprs.truncate(actual_size);
            self.ctfe_consts.truncate(actual_size);
            self.type_map.truncate(actual_size);
        }

        OptimizedMir {
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
            type_map: std::mem::take(&mut self.type_map),
        }
    }

    fn lower_ast(&mut self, ast: &AstNode) {
        // Simplified implementation - full version would match the original
        match ast {
            AstNode::Let { pattern, expr, .. } => {
                if let AstNode::Var(name) = &**pattern {
                    let rhs_id = self.lower_expr(expr);
                    let lhs_id = self.next_id();
                    self.stmts.push(MirStmt::Assign {
                        lhs: lhs_id,
                        rhs: rhs_id,
                    });
                    self.set_name_id(name.clone(), lhs_id);
                    self.exprs[lhs_id as usize] = Some(MirExpr::Var(lhs_id));
                    self.type_map[lhs_id as usize] = Some(Type::I64);
                }
            }
            AstNode::Return(inner) => {
                let val = self.lower_expr(inner);
                self.stmts.push(MirStmt::Return { val });
            }
            AstNode::ExprStmt { expr } => {
                let _ = self.lower_expr(expr);
            }
            _ => {
                // For other nodes, process based on type
                match ast {
                    AstNode::FuncDef { body, .. } => {
                        for child in body {
                            self.lower_ast(child);
                        }
                    }
                    AstNode::BinaryOp { left, right, .. } => {
                        self.lower_ast(left);
                        self.lower_ast(right);
                    }
                    AstNode::Call { args, .. } => {
                        for arg in args {
                            self.lower_ast(arg);
                        }
                    }
                    AstNode::If {
                        cond, then, else_, ..
                    } => {
                        self.lower_ast(cond);
                        for stmt in then {
                            self.lower_ast(stmt);
                        }
                        for stmt in else_ {
                            self.lower_ast(stmt);
                        }
                    }
                    _ => {
                        // No children to process
                    }
                }
            }
        }
    }

    fn lower_expr(&mut self, expr: &AstNode) -> u32 {
        match expr {
            AstNode::Var(name) => {
                if let Some(id) = self.get_name_id(name) {
                    id
                } else {
                    // Global or undefined - create a new ID
                    let id = self.next_id();
                    self.exprs[id as usize] = Some(MirExpr::Var(id));
                    self.type_map[id as usize] = Some(Type::I64);
                    id
                }
            }
            AstNode::Lit(value) => self.next_id_with_lit(*value),
            AstNode::BinaryOp { op, left, right } => {
                let left_id = self.lower_expr(left);
                let right_id = self.lower_expr(right);
                let result_id = self.next_id();

                // For now, just create a call to the operator
                self.stmts.push(MirStmt::Call {
                    func: format!("operator_{}", op),
                    args: vec![left_id, right_id],
                    dest: result_id,
                    type_args: vec![],
                });

                self.exprs[result_id as usize] = Some(MirExpr::Var(result_id));
                self.type_map[result_id as usize] = Some(Type::I64);
                result_id
            }
            _ => {
                // Default: create a placeholder
                self.next_id_with_lit(0)
            }
        }
    }
}
