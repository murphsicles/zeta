// src/middle/mir/gen.rs
use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use std::collections::{HashMap, VecDeque};

pub struct MirGen {
    next_id: u32,
    stmts: Vec<MirStmt>,
    exprs: HashMap<u32, MirExpr>,
    ctfe_consts: HashMap<u32, i64>,
    #[allow(dead_code)]
    defers: Vec<u32>,
    defer_stmts: Vec<MirStmt>,
}

impl MirGen {
    pub fn new() -> Self {
        Self {
            next_id: 0,
            stmts: vec![],
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
            defers: vec![],
            defer_stmts: vec![],
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

    pub fn finalize_mir(mut self) -> Mir {
        Mir {
            name: None,
            param_indices: vec![],
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

                let op_kind = if op == "+" {
                    SemiringOp::Add
                } else if op == "*" {
                    SemiringOp::Mul
                } else {
                    self.stmts.push(MirStmt::Call {
                        func: op.clone(),
                        args: vec![left_id, right_id],
                        dest,
                        type_args: vec![],
                    });
                    return;
                };

                self.stmts.push(MirStmt::SemiringFold {
                    op: op_kind,
                    values: vec![left_id, right_id],
                    result: dest,
                });
            }
            AstNode::TryProp { expr } => {
                let expr_id = self.lower_expr(expr);
                let ok_dest = self.next_id();
                let err_dest = self.next_id();
                self.stmts.push(MirStmt::TryProp {
                    expr_id,
                    ok_dest,
                    err_dest,
                });
            }
            AstNode::DictLit { entries } => {
                let map_id = self.next_id();
                self.stmts.push(MirStmt::MapNew { dest: map_id });

                for (key, val) in entries {
                    let key_id = self.lower_expr(key);
                    let val_id = self.lower_expr(val);
                    self.stmts.push(MirStmt::DictInsert {
                        map_id,
                        key_id,
                        val_id,
                    });
                }

                self.exprs.insert(map_id, MirExpr::Var(map_id));
            }
            AstNode::Subscript { base, index } => {
                let base_id = self.lower_expr(base);
                let index_id = self.lower_expr(index);
                let dest = self.next_id();
                self.stmts.push(MirStmt::DictGet {
                    map_id: base_id,
                    key_id: index_id,
                    dest,
                });
            }
            AstNode::Defer(inner) => {
                let mut subgen = MirGen::new();
                subgen.lower_ast(inner);
                let cleanup_stmts = subgen.finalize_mir().stmts;
                self.defer_stmts.extend(cleanup_stmts);
            }
            AstNode::FuncDef { body, .. } => {
                let mut collected_defers = VecDeque::new();
                for stmt in body {
                    if let AstNode::Defer(_) = stmt {
                        collected_defers.push_front(stmt.clone());
                    }
                    self.lower_ast(stmt);
                }

                for defer_ast in collected_defers {
                    let mut subgen = MirGen::new();
                    subgen.lower_ast(&defer_ast);
                    let cleanup = subgen.finalize_mir().stmts;
                    self.stmts.extend(cleanup);
                }
            }
            AstNode::If { cond, then, else_ } => {
                let cond_id = self.lower_expr(cond);
                let mut then_stmts = vec![];
                for s in then {
                    let mut r#gen = MirGen::new();
                    r#gen.lower_ast(s);
                    then_stmts.extend(r#gen.stmts);
                }
                let mut else_stmts = vec![];
                for s in else_ {
                    let mut r#gen = MirGen::new();
                    r#gen.lower_ast(s);
                    else_stmts.extend(r#gen.stmts);
                }
                self.stmts.push(MirStmt::If {
                    cond: cond_id,
                    then: then_stmts,
                    else_: else_stmts,
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
                let ids = parts.iter().map(|p| self.lower_expr(p)).collect();
                MirExpr::FString(ids)
            }
            AstNode::TimingOwned { inner, .. } => MirExpr::TimingOwned(self.lower_expr(inner)),
            AstNode::DictLit { .. } => MirExpr::Var(id),
            AstNode::Subscript { .. } => MirExpr::Var(id),
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
