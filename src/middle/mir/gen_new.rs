// src/middle/mir/gen_new.rs
//! Enhanced MIR generation with type information

use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt};
use crate::middle::types::Type;
use std::collections::HashMap;

pub struct MirGenWithTypes {
    next_id: u32,
    stmts: Vec<MirStmt>,
    exprs: HashMap<u32, MirExpr>,
    type_map: HashMap<u32, Type>,
    name_to_id: HashMap<String, u32>,
}

impl MirGenWithTypes {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            stmts: vec![],
            exprs: HashMap::new(),
            type_map: HashMap::new(),
            name_to_id: HashMap::new(),
        }
    }

    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn next_id_with_lit(&mut self, value: i64) -> u32 {
        let id = self.next_id();
        self.exprs.insert(id, MirExpr::Lit(value));
        self.type_map.insert(id, Type::I64);
        id
    }

    /// Lower AST to MIR with type information
    pub fn lower_to_mir(&mut self, ast: &AstNode, type_info: &HashMap<String, Type>) -> Mir {
        self.name_to_id.clear();
        self.stmts.clear();
        self.exprs.clear();
        self.type_map.clear();
        self.next_id = 1;

        // Initialize type map with provided type information
        // Note: We need to map from variable names to IDs and types

        if let AstNode::FuncDef { params, .. } = ast {
            for (i, (name, type_str)) in params.iter().enumerate() {
                let id = self.next_id();
                self.name_to_id.insert(name.clone(), id);
                self.exprs.insert(id, MirExpr::Var(id));
                
                // Get type from type_info or convert from string
                let param_type = if let Some(ty) = type_info.get(name) {
                    ty.clone()
                } else {
                    // Convert string type to Type enum
                    match type_str.as_str() {
                        "i8" => Type::I8,
                        "i16" => Type::I16,
                        "i32" => Type::I32,
                        "i64" => Type::I64,
                        "u8" => Type::U8,
                        "u16" => Type::U16,
                        "u32" => Type::U32,
                        "u64" => Type::U64,
                        "f32" => Type::F32,
                        "f64" => Type::F64,
                        "bool" => Type::Bool,
                        "char" => Type::Char,
                        "str" => Type::Str,
                        "()" => Type::Unit,
                        _ => Type::Named(type_str.clone(), vec![]),
                    }
                };
                
                self.type_map.insert(id, param_type);
                self.stmts.push(MirStmt::ParamInit {
                    param_id: id,
                    arg_index: i as u32,
                });
            }
        }

        // TODO: Implement full AST lowering with type information
        // For now, just create a simple MIR
        self.lower_ast_simple(ast);

        if self.stmts.is_empty() || !matches!(self.stmts.last(), Some(MirStmt::Return { .. })) {
            let ret_val = self.next_id_with_lit(0);
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
            ctfe_consts: HashMap::new(),
            type_map: std::mem::take(&mut self.type_map),
        }
    }

    fn lower_ast_simple(&mut self, ast: &AstNode) {
        match ast {
            AstNode::Let { pattern, expr, .. } => {
                if let AstNode::Var(name) = &**pattern {
                    let rhs_id = self.next_id_with_lit(0); // Placeholder
                    let lhs_id = self.next_id();
                    self.stmts.push(MirStmt::Assign {
                        lhs: lhs_id,
                        rhs: rhs_id,
                    });
                    self.name_to_id.insert(name.clone(), lhs_id);
                    self.exprs.insert(lhs_id, MirExpr::Var(lhs_id));
                    // Copy type from RHS to LHS
                    if let Some(rhs_type) = self.type_map.get(&rhs_id) {
                        self.type_map.insert(lhs_id, rhs_type.clone());
                    }
                }
            }
            AstNode::Return(expr) => {
                let ret_id = self.next_id_with_lit(0); // Placeholder
                self.stmts.push(MirStmt::Return { val: ret_id });
            }
            _ => {
                // For other nodes, create a simple expression
                let expr_id = self.next_id_with_lit(0);
                self.stmts.push(MirStmt::Assign {
                    lhs: expr_id,
                    rhs: expr_id,
                });
            }
        }
    }
}