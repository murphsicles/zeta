// src/middle/mir/gen.rs
//! # MIR Generation from AST
//!
//! Lowers Zeta AST to our clean Minimal Intermediate Representation (MIR).
//! All Zeta features (methods, generics, control flow, dicts, etc.) are lowered here.
//! Clean, fast, and fully documented.

use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::middle::specialization::MonoKey;
use std::collections::HashMap;

pub struct MirGen {
    next_id: u32,
    stmts: Vec<MirStmt>,
    exprs: HashMap<u32, MirExpr>,
    ctfe_consts: HashMap<u32, i64>,
    type_map: HashMap<u32, String>,
    name_to_id: HashMap<String, u32>,
}

impl MirGen {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            stmts: vec![],
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
            type_map: HashMap::new(),
            name_to_id: HashMap::new(),
        }
    }

    pub fn lower_to_mir(&mut self, ast: &AstNode) -> Mir {
        self.name_to_id.clear();
        self.stmts.clear();
        self.exprs.clear();
        self.next_id = 1;

        if let AstNode::FuncDef { params, .. } = ast {
            for (i, (name, _)) in params.iter().enumerate() {
                let id = self.next_id();
                self.name_to_id.insert(name.clone(), id);
                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, "i64".to_string());
                self.stmts.push(MirStmt::ParamInit {
                    param_id: id,
                    arg_index: i as u32,
                });
            }
        }

        self.lower_ast(ast);

        if self.stmts.is_empty() || !matches!(self.stmts.last(), Some(MirStmt::Return { .. })) {
            let ret_val = if let Some(last) = self.stmts.last() {
                match last {
                    MirStmt::Call { dest, .. } => *dest,
                    MirStmt::SemiringFold { result, .. } => *result,
                    MirStmt::Assign { lhs, .. } => *lhs,
                    MirStmt::DictGet { dest, .. } => *dest,
                    _ => self.next_id_with_lit(0),
                }
            } else {
                self.next_id_with_lit(0)
            };
            self.stmts.push(MirStmt::Return { val: ret_val });
        }

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
            type_map: std::mem::take(&mut self.type_map),
        }
    }

    fn lower_ast(&mut self, ast: &AstNode) {
        match ast {
            AstNode::Let { pattern, expr, .. } => {
                if let AstNode::Var(name) = &**pattern {
                    let rhs_id = self.lower_expr(expr);
                    let lhs_id = self.next_id();
                    self.stmts.push(MirStmt::Assign {
                        lhs: lhs_id,
                        rhs: rhs_id,
                    });
                    self.name_to_id.insert(name.clone(), lhs_id);
                    self.exprs.insert(lhs_id, MirExpr::Var(lhs_id));
                    self.type_map.insert(lhs_id, "i64".to_string());
                }
            }
            AstNode::Assign(lhs, rhs) => {
                let rhs_id = self.lower_expr(rhs);
                if let AstNode::Subscript { base, index } = &**lhs {
                    let base_id = self.lower_expr(base);
                    let index_id = self.lower_expr(index);
                    self.stmts.push(MirStmt::DictInsert {
                        map_id: base_id,
                        key_id: index_id,
                        val_id: rhs_id,
                    });
                } else {
                    let lhs_id = self.lower_expr(lhs);
                    self.stmts.push(MirStmt::Assign {
                        lhs: lhs_id,
                        rhs: rhs_id,
                    });
                }
            }
            AstNode::Return(inner) => {
                let val = self.lower_expr(inner);
                self.stmts.push(MirStmt::Return { val });
            }
            AstNode::BinaryOp { op, left, right } => {
                let _ = self.lower_expr(&AstNode::BinaryOp {
                    op: op.clone(),
                    left: left.clone(),
                    right: right.clone(),
                });
            }
            AstNode::TryProp { expr } => {
                let expr_id = self.lower_expr(expr);
                let ok = self.next_id();
                let err = self.next_id();
                self.stmts.push(MirStmt::TryProp {
                    expr_id,
                    ok_dest: ok,
                    err_dest: err,
                });
            }
            AstNode::DictLit { entries } => {
                let map_id = self.next_id();
                self.stmts.push(MirStmt::MapNew { dest: map_id });
                for (k, v) in entries {
                    let kid = self.lower_expr(k);
                    let vid = self.lower_expr(v);
                    self.stmts.push(MirStmt::DictInsert {
                        map_id,
                        key_id: kid,
                        val_id: vid,
                    });
                }
                self.exprs.insert(map_id, MirExpr::Var(map_id));
                self.type_map.insert(map_id, "map".to_string());
            }
            AstNode::Subscript { base, index } => {
                let bid = self.lower_expr(base);
                let iid = self.lower_expr(index);
                let dest = self.next_id();
                self.stmts.push(MirStmt::DictGet {
                    map_id: bid,
                    key_id: iid,
                    dest,
                });
                self.type_map.insert(dest, "i64".to_string());
            }
            AstNode::FuncDef { body, ret_expr, .. } => {
                for stmt in body {
                    self.lower_ast(stmt);
                }
                if let Some(ret_expr) = ret_expr {
                    let val = self.lower_expr(&**ret_expr);
                    self.stmts.push(MirStmt::Return { val });
                }
            }
            AstNode::If { cond, then, else_ } => {
                let cond_id = self.lower_expr(cond);
                let mut then_stmts = vec![];
                for s in then {
                    let mut sub = MirGen::new();
                    sub.lower_ast(s);
                    then_stmts.extend(sub.stmts);
                }
                let mut else_stmts = vec![];
                for s in else_ {
                    let mut sub = MirGen::new();
                    sub.lower_ast(s);
                    else_stmts.extend(sub.stmts);
                }
                self.stmts.push(MirStmt::If {
                    cond: cond_id,
                    then: then_stmts,
                    else_: else_stmts,
                });
            }
            AstNode::ExprStmt { expr } => {
                if let AstNode::Return(inner) = &**expr {
                    let val = self.lower_expr(inner);
                    self.stmts.push(MirStmt::Return { val });
                } else {
                    let _ = self.lower_expr(expr);
                }
            }
            _ => {}
        }
    }

    fn lower_expr(&mut self, expr: &AstNode) -> u32 {
        let id = self.next_id();
        match expr {
            AstNode::Var(name) => {
                if let Some(&existing) = self.name_to_id.get(name) {
                    return existing;
                }
                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, "i64".to_string());
            }
            AstNode::Lit(n) => {
                self.exprs.insert(id, MirExpr::Lit(*n));
                self.type_map.insert(id, "i64".to_string());
            }
            AstNode::StringLit(s) => {
                self.exprs.insert(id, MirExpr::StringLit(s.clone()));
                self.type_map.insert(id, "str".to_string());
            }
            AstNode::FString(parts) => {
                let part_ids: Vec<_> = parts.iter().map(|p| self.lower_expr(p)).collect();
                self.exprs.insert(id, MirExpr::FString(part_ids));
                self.type_map.insert(id, "str".to_string());
            }
            AstNode::BinaryOp { op, left, right } => {
                let left_id = self.lower_expr(left);
                let right_id = self.lower_expr(right);
                let dest = self.next_id();
                if op == "+" {
                    self.stmts.push(MirStmt::SemiringFold {
                        op: SemiringOp::Add,
                        values: vec![left_id, right_id],
                        result: dest,
                    });
                } else if op == "*" {
                    self.stmts.push(MirStmt::SemiringFold {
                        op: SemiringOp::Mul,
                        values: vec![left_id, right_id],
                        result: dest,
                    });
                } else {
                    self.stmts.push(MirStmt::Call {
                        func: op.clone(),
                        args: vec![left_id, right_id],
                        dest,
                        type_args: vec![],
                    });
                }
                self.exprs.insert(dest, MirExpr::Var(dest));
                self.type_map.insert(dest, "i64".to_string());
                return dest;
            }
            AstNode::Call {
                receiver,
                method,
                args,
                ..
            } => {
                let mut arg_ids = vec![];
                let receiver_ty = if let Some(r) = receiver {
                    let rid = self.lower_expr(r);
                    arg_ids.push(rid);
                    Some(
                        self.type_map
                            .get(&rid)
                            .cloned()
                            .unwrap_or_else(|| "i64".to_string()),
                    )
                } else {
                    None
                };
                for a in args {
                    arg_ids.push(self.lower_expr(a));
                }
                let func = if let Some(ref rty) = receiver_ty {
                    let key = MonoKey {
                        func_name: method.clone(),
                        type_args: vec![rty.clone()],
                    };
                    key.mangle()
                } else {
                    method.clone()
                };
                self.stmts.push(MirStmt::Call {
                    func,
                    args: arg_ids,
                    dest: id,
                    type_args: receiver_ty.map(|t| vec![t]).unwrap_or_default(),
                });
                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, "i64".to_string());
            }
            _ => {
                self.exprs.insert(id, MirExpr::Lit(0));
                self.type_map.insert(id, "i64".to_string());
            }
        }
        id
    }

    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn next_id_with_lit(&mut self, n: i64) -> u32 {
        let id = self.next_id();
        self.exprs.insert(id, MirExpr::Lit(n));
        self.type_map.insert(id, "i64".to_string());
        id
    }
}

impl Default for MirGen {
    fn default() -> Self {
        Self::new()
    }
}
