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
    type_map: HashMap<u32, String>,
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
            type_map: HashMap::new(),
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
            type_map: std::mem::take(&mut self.type_map),
        }
    }

    pub fn finalize_mir(mut self) -> Mir {
        Mir {
            name: None,
            param_indices: vec![],
            stmts: std::mem::take(&mut self.stmts),
            exprs: std::mem::take(&mut self.exprs),
            ctfe_consts: std::mem::take(&mut self.ctfe_consts),
            type_map: std::mem::take(&mut self.type_map),
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
                let lty = self.type_map[&left_id].clone();
                let rty = self.type_map[&right_id].clone();

                if op == "+" && lty == "str" && rty == "str" {
                    self.stmts.push(MirStmt::Call {
                        func: "host_str_concat".to_string(),
                        args: vec![left_id, right_id],
                        dest,
                        type_args: vec![],
                    });
                    self.type_map.insert(dest, "str".to_string());
                } else if lty == "i64" && rty == "i64" {
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
                    self.type_map.insert(dest, "i64".to_string());
                } else {
                    // Fallback / error case – treat as unknown call (will fail later)
                    self.stmts.push(MirStmt::Call {
                        func: op.clone(),
                        args: vec![left_id, right_id],
                        dest,
                        type_args: vec![],
                    });
                    self.type_map.insert(dest, "unknown".to_string());
                }

                self.exprs.insert(dest, MirExpr::Var(dest));
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
                self.type_map.insert(map_id, "map".to_string());
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
                self.type_map.insert(dest, "i64".to_string());
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
            AstNode::ExprStmt { expr } => {
                // Expression statement – evaluate for side-effects, discard result
                let _ = self.lower_expr(expr);
            }
            _ => {}
        }
    }

    fn lower_expr(&mut self, expr: &AstNode) -> u32 {
        let id = self.next_id();

        match expr {
            AstNode::Var(_name) => {
                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, "i64".to_string()); // Conservative default
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
                let part_ids = parts.iter().map(|p| self.lower_expr(p)).collect();
                self.exprs.insert(id, MirExpr::FString(part_ids));
                self.type_map.insert(id, "str".to_string());
            }
            AstNode::TimingOwned { inner, .. } => {
                let inner_id = self.lower_expr(inner);
                self.exprs.insert(id, MirExpr::TimingOwned(inner_id));
                self.type_map.insert(id, "i64".to_string());
            }
            AstNode::DictLit { .. } => {
                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, "map".to_string());
            }
            AstNode::Subscript { .. } => {
                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, "i64".to_string());
            }
            AstNode::Call { receiver, method, args, type_args, structural: _ } => {
                let mut arg_ids = Vec::new();
                let receiver_ty = if let Some(recv) = receiver {
                    let recv_id = self.lower_expr(recv);
                    arg_ids.push(recv_id);
                    Some(self.type_map[&recv_id].clone())
                } else {
                    None
                };

                for arg in args {
                    let arg_id = self.lower_expr(arg);
                    arg_ids.push(arg_id);
                }

                let func = if let Some(ref rty) = receiver_ty {
                    if rty == "str" {
                        match method.as_str() {
                            "to_lowercase" => "host_str_to_lowercase".to_string(),
                            "to_uppercase" => "host_str_to_uppercase".to_string(),
                            "len" => "host_str_len".to_string(),
                            "starts_with" => "host_str_starts_with".to_string(),
                            "ends_with" => "host_str_ends_with".to_string(),
                            "contains" => "host_str_contains".to_string(),
                            "trim" => "host_str_trim".to_string(),
                            "replace" => "host_str_replace".to_string(),
                            _ => method.clone(),
                        }
                    } else {
                        method.clone()
                    }
                } else {
                    method.clone()
                };

                self.stmts.push(MirStmt::Call {
                    func,
                    args: arg_ids,
                    dest: id,
                    type_args: type_args.clone(),
                });

                let ret_ty = if receiver_ty.as_ref().map_or(false, |rty| rty == "str") {
                    match method.as_str() {
                        "starts_with" | "ends_with" | "contains" => "i64".to_string(),
                        "len" => "i64".to_string(),
                        _ => "str".to_string(),
                    }
                } else {
                    "i64".to_string()
                };
                self.type_map.insert(id, ret_ty);
                self.exprs.insert(id, MirExpr::Var(id));
            }
            AstNode::PathCall { path: _, method, args } => {
                // Simple path call lowering – treat as normal call for now
                let mut arg_ids = Vec::new();
                for arg in args {
                    arg_ids.push(self.lower_expr(arg));
                }
                self.stmts.push(MirStmt::Call {
                    func: method.clone(),
                    args: arg_ids,
                    dest: id,
                    type_args: vec![],
                });
                self.type_map.insert(id, "i64".to_string());
                self.exprs.insert(id, MirExpr::Var(id));
            }
            AstNode::Spawn { func: _, args } => {
                let mut arg_ids = Vec::new();
                for arg in args {
                    arg_ids.push(self.lower_expr(arg));
                }
                self.stmts.push(MirStmt::Call {
                    func: "spawn".to_string(),
                    args: arg_ids,
                    dest: id,
                    type_args: vec![],
                });
                self.type_map.insert(id, "i64".to_string());
                self.exprs.insert(id, MirExpr::Var(id));
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
}

impl Default for MirGen {
    fn default() -> Self {
        Self::new()
    }
}
