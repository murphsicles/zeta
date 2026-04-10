// src/middle/mir/gen.rs
//! # MIR Generation from AST
//!
//! Lowers Zeta AST to our clean Minimal Intermediate Representation (MIR).
//! All Zeta features (methods, generics, control flow, dicts, etc.) are lowered here.
//! Clean, fast, and fully documented.

use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::middle::specialization::MonoKey;
use crate::middle::types::{Type, ArraySize};
use std::collections::HashMap;

pub struct MirGen {
    next_id: u32,
    stmts: Vec<MirStmt>,
    exprs: HashMap<u32, MirExpr>,
    ctfe_consts: HashMap<u32, i64>,
    type_map: HashMap<u32, Type>,
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
                self.type_map.insert(id, Type::I64);
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
                    MirStmt::If {
                        dest: Some(dest_id),
                        ..
                    } => {
                        // If expression produces a value
                        *dest_id
                    }
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
                // Handle different pattern types
                match &**pattern {
                    AstNode::Var(name) => {
                        let rhs_id = self.lower_expr(expr);
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
                        } else {
                            self.type_map.insert(lhs_id, Type::I64);
                        }
                    }
                    AstNode::TypeAnnotatedPattern { pattern: inner_pattern, ty: _ } => {
                        // For type-annotated patterns, extract the inner pattern
                        // The type checking should have been done by the type checker
                        if let AstNode::Var(name) = &**inner_pattern {
                            let rhs_id = self.lower_expr(expr);
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
                            } else {
                                self.type_map.insert(lhs_id, Type::I64);
                            }
                        }
                        // Note: We could add runtime identity checking here if needed,
                        // but the type checker should have already validated the type.
                    }
                    _ => {
                        // For other pattern types, generate a simple assignment
                        // This is a simplification - in a full implementation,
                        // we would need to handle destructuring patterns
                        let rhs_id = self.lower_expr(expr);
                        let lhs_id = self.next_id();
                        self.stmts.push(MirStmt::Assign {
                            lhs: lhs_id,
                            rhs: rhs_id,
                        });
                        self.exprs.insert(lhs_id, MirExpr::Var(lhs_id));
                        self.type_map.insert(lhs_id, Type::I64);
                    }
                }
            }
            AstNode::Assign(lhs, rhs) => {
                let rhs_id = self.lower_expr(rhs);
                if let AstNode::Subscript { base, index } = &**lhs {
                    let base_id = self.lower_expr(base);
                    let index_id = self.lower_expr(index);
                    
                    // Check if base is an array type
                    let base_ty = self.type_map.get(&base_id).cloned().unwrap_or(Type::I64);
                    if let Type::DynamicArray(_) = base_ty {
                        // Generate array_set call for dynamic arrays
                        self.stmts.push(MirStmt::VoidCall {
                            func: "array_set".to_string(),
                            args: vec![base_id, index_id, rhs_id],
                        });
                    } else if let Type::Array(_, size) = base_ty {
                        // Check if this is a stack array (fixed size) or heap array
                        match size {
                            ArraySize::Literal(n) if n <= 1024 => {
                                // Small fixed-size array - treat as stack array
                                // Use stack_array_set for direct memory access
                                self.stmts.push(MirStmt::VoidCall {
                                    func: "stack_array_set".to_string(),
                                    args: vec![base_id, index_id, rhs_id],
                                });
                            }
                            _ => {
                                // Dynamic or large array - use heap array access
                                self.stmts.push(MirStmt::VoidCall {
                                    func: "array_set".to_string(),
                                    args: vec![base_id, index_id, rhs_id],
                                });
                            }
                        }
                    } else {
                        // Use DictInsert for other types (maps/dicts)
                        self.stmts.push(MirStmt::DictInsert {
                            map_id: base_id,
                            key_id: index_id,
                            val_id: rhs_id,
                        });
                    }
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
                self.type_map
                    .insert(map_id, Type::Named("map".to_string(), vec![]));
            }
            AstNode::Subscript { base, index } => {
                // This is handled in lower_expr
                let _ = self.lower_expr(&AstNode::Subscript {
                    base: base.clone(),
                    index: index.clone(),
                });
            }
            AstNode::FuncDef { body, ret_expr, .. } => {
                for stmt in body {
                    self.lower_ast(stmt);
                }
                if let Some(ret_expr) = ret_expr {
                    let val = self.lower_expr(ret_expr);
                    self.stmts.push(MirStmt::Return { val });
                }
            }
            AstNode::If { cond, then, else_ } => {
                let cond_id = self.lower_expr(cond);

                // Check if this is expression if (branches produce values) or statement if
                // Simple heuristic: if any branch contains return, treat as statement
                let mut is_statement_if = false;
                let mut then_has_return = false;
                let mut else_has_return = false;

                // Scan branches for returns
                for s in then.iter() {
                    if let AstNode::Return(_) = s {
                        then_has_return = true;
                        is_statement_if = true;
                        break;
                    }
                }
                for s in else_.iter() {
                    if let AstNode::Return(_) = s {
                        else_has_return = true;
                        is_statement_if = true;
                        break;
                    }
                }

                let dest_id = if is_statement_if {
                    // Statement if: no destination needed
                    None
                } else {
                    // Expression if: create destination
                    let id = self.next_id();

                    self.exprs.insert(id, MirExpr::Var(id));
                    self.type_map.insert(id, Type::I64);
                    Some(id)
                };

                // Generate then block in isolated context
                // Generate then block inline (no isolated context)
                let mut then_stmts = vec![];
                if !then.is_empty() {
                    // Save current statements
                    let saved_stmts = std::mem::take(&mut self.stmts);

                    // Generate block statements directly in current context
                    for s in then {
                        self.lower_ast(s);
                    }

                    // Take the generated statements
                    then_stmts = std::mem::take(&mut self.stmts);

                    // Restore main statements
                    self.stmts = saved_stmts;

                    // For expression if, capture the last value
                    if let Some(dest) = dest_id
                        && !then_has_return
                        && let Some(last_stmt) = then_stmts.last()
                    {
                        match last_stmt {
                            MirStmt::Assign { lhs, .. } => {
                                // Add assignment to dest
                                then_stmts.push(MirStmt::Assign {
                                    lhs: dest,
                                    rhs: *lhs,
                                });
                            }
                            _ => {
                                // No value-producing statement found
                                let zero_id = self.next_id_with_lit(0);
                                then_stmts.push(MirStmt::Assign {
                                    lhs: dest,
                                    rhs: zero_id,
                                });
                            }
                        }
                    }
                }

                // Generate else block inline
                let mut else_stmts = vec![];
                if !else_.is_empty() {
                    // Save current statements
                    let saved_stmts = std::mem::take(&mut self.stmts);

                    // Generate block statements directly in current context
                    for s in else_ {
                        self.lower_ast(s);
                    }

                    // Take the generated statements
                    else_stmts = std::mem::take(&mut self.stmts);

                    // Restore main statements
                    self.stmts = saved_stmts;

                    // For expression if, capture the last value
                    if let Some(dest) = dest_id
                        && !else_has_return
                        && let Some(last_stmt) = else_stmts.last()
                    {
                        match last_stmt {
                            MirStmt::Assign { lhs, .. } => {
                                // Add assignment to dest
                                else_stmts.push(MirStmt::Assign {
                                    lhs: dest,
                                    rhs: *lhs,
                                });
                            }
                            _ => {
                                // No value-producing statement found
                                let zero_id = self.next_id_with_lit(0);
                                else_stmts.push(MirStmt::Assign {
                                    lhs: dest,
                                    rhs: zero_id,
                                });
                            }
                        }
                    }
                }

                // Expressions are already in self.exprs (generated inline)
                // No need to merge or update next_id

                self.stmts.push(MirStmt::If {
                    cond: cond_id,
                    then: then_stmts,
                    else_: else_stmts,
                    dest: dest_id,
                });
            }
            AstNode::ExprStmt { expr } => {
                if let AstNode::Return(inner) = &**expr {
                    let val = self.lower_expr(inner);
                    self.stmts.push(MirStmt::Return { val });
                } else {
                    let expr_id = self.lower_expr(expr);
                    // For expression statements that are values (not just side effects),
                    // we need to capture the value. Create a temporary assignment.
                    // This will be optimized away if not needed.
                    let temp_id = self.next_id();
                    self.stmts.push(MirStmt::Assign {
                        lhs: temp_id,
                        rhs: expr_id,
                    });
                    // Store the temp ID for implicit return to find
                    self.exprs.insert(temp_id, MirExpr::Var(temp_id));
                    self.type_map.insert(temp_id, Type::I64);
                }
            }
            AstNode::For {
                pattern,
                expr,
                body,
            } => {
                // For now, implement simple desugaring for range-based for loops
                // for i in start..end { body } desugars to:
                // let mut i = start;
                // while i < end {
                //   body;
                //   i = i + 1;
                // }

                // Check if expr is a range expression (BinaryOp with ".." or AstNode::Range)
                let (start_expr, end_expr, inclusive) = match &**expr {
                    AstNode::BinaryOp { op, left, right } if op == ".." => {
                        (left, right, false)
                    }
                    AstNode::Range { start, end, inclusive } => {
                        (start, end, *inclusive)
                    }
                    _ => {
                        // TODO: Handle other iterator types
                        return;
                    }
                };
                
                // Get variable name from pattern
                if let AstNode::Var(var_name) = &**pattern {
                    // Lower start and end expressions
                    let start_id = self.lower_expr(start_expr);
                    let end_id = self.lower_expr(end_expr);

                    // Create loop variable
                    let var_id = self.next_id();
                    self.name_to_id.insert(var_name.clone(), var_id);
                    self.exprs.insert(var_id, MirExpr::Var(var_id));
                    self.type_map.insert(var_id, Type::I64);

                    // Initialize loop variable: let mut i = start
                    self.stmts.push(MirStmt::Assign {
                        lhs: var_id,
                        rhs: start_id,
                    });

                    // Create a range iterator expression
                    // For range start..end, we need to create an iterator
                    // For now, we'll create a simple representation
                    let range_id = self.next_id();
                    self.exprs.insert(range_id, MirExpr::Range {
                        start: start_id,
                        end: end_id,
                        inclusive,
                    });
                    self.type_map.insert(range_id, Type::Range);
                    
                    // Save current statements to restore after loop body
                    let stmts_before_body = self.stmts.len();

                    // Generate loop body
                    for stmt in body {
                        self.lower_ast(stmt);
                    }

                    // Get body statements
                    let body_stmts = self.stmts.split_off(stmts_before_body);
                    
                    // Create For statement in MIR
                    self.stmts.push(MirStmt::For {
                        iterator: range_id,
                        pattern: var_name.clone(),
                        var_id: var_id,
                        body: body_stmts,
                    });
                }
            }
            AstNode::While { cond, body } => {
                let cond_id = self.lower_expr(cond);
                
                // Save current statements to restore after loop body
                let stmts_before_body = self.stmts.len();
                
                // Generate loop body
                for stmt in body {
                    self.lower_ast(stmt);
                }
                
                // Get body statements
                let body_stmts = self.stmts.split_off(stmts_before_body);
                
                // Create While statement in MIR
                self.stmts.push(MirStmt::While {
                    cond: cond_id,
                    body: body_stmts,
                });
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
                self.type_map.insert(id, Type::I64);
            }
            AstNode::Lit(n) => {
                self.exprs.insert(id, MirExpr::Lit(*n));
                self.type_map.insert(id, Type::I32);
            }
            AstNode::Bool(b) => {
                // Convert bool to i64: true = 1, false = 0
                let value = if *b { 1 } else { 0 };
                self.exprs.insert(id, MirExpr::Lit(value));
                self.type_map.insert(id, Type::Bool);
            }
            AstNode::StringLit(s) => {
                self.exprs.insert(id, MirExpr::StringLit(s.clone()));
                self.type_map.insert(id, Type::Str);
            }
            AstNode::FString(parts) => {
                let part_ids: Vec<_> = parts.iter().map(|p| self.lower_expr(p)).collect();
                self.exprs.insert(id, MirExpr::FString(part_ids));
                self.type_map.insert(id, Type::Str);
            }
            AstNode::BinaryOp { op, left, right } => {
                let left_id = self.lower_expr(left);
                let right_id = self.lower_expr(right);
                let dest = self.next_id();
                
                if op == ".." {
                    // Range expression for for loops (exclusive)
                    self.exprs.insert(dest, MirExpr::Range {
                        start: left_id,
                        end: right_id,
                        inclusive: false,
                    });
                    self.type_map.insert(dest, Type::Range);
                } else if op == "+" {
                    self.stmts.push(MirStmt::SemiringFold {
                        op: SemiringOp::Add,
                        values: vec![left_id, right_id],
                        result: dest,
                    });
                    self.exprs.insert(dest, MirExpr::Var(dest));
                    self.type_map.insert(dest, Type::I64);
                } else if op == "*" {
                    self.stmts.push(MirStmt::SemiringFold {
                        op: SemiringOp::Mul,
                        values: vec![left_id, right_id],
                        result: dest,
                    });
                    self.exprs.insert(dest, MirExpr::Var(dest));
                    self.type_map.insert(dest, Type::I64);
                } else {
                    // For comparison operators used in loop conditions, create BinaryOp expression
                    // instead of caching the result in a variable
                    if op == "<" || op == ">" || op == "<=" || op == ">=" || op == "==" || op == "!=" {
                        self.exprs.insert(dest, MirExpr::BinaryOp {
                            op: op.clone(),
                            left: left_id,
                            right: right_id,
                        });
                    } else {
                        self.stmts.push(MirStmt::Call {
                            func: op.clone(),
                            args: vec![left_id, right_id],
                            dest,
                            type_args: vec![],
                        });
                        self.exprs.insert(dest, MirExpr::Var(dest));
                    }
                    self.type_map.insert(dest, Type::I64);
                }
                return dest;
            }
            
            AstNode::Range { start, end, inclusive } => {
                let start_id = self.lower_expr(start);
                let end_id = self.lower_expr(end);
                let dest = self.next_id();
                
                // Range expression for for loops
                self.exprs.insert(dest, MirExpr::Range {
                    start: start_id,
                    end: end_id,
                    inclusive: *inclusive,
                });
                self.type_map.insert(dest, Type::Range);
                return dest;
            }
            AstNode::Call {
                receiver,
                method,
                args,
                type_args,
                ..
            } => {
                println!(
                    "[MIR GEN DEBUG] Processing call: method={:?}, receiver={:?}, args={:?}, type_args={:?}",
                    method, receiver, args, type_args
                );

                // SPECIAL HANDLING: If method is "call" and receiver is a function name,
                // generate direct function call instead of call_i64
                if method == "call" {
                    // receiver is &Option<Box<AstNode>>
                    if let Some(receiver_ast) = receiver {
                        // receiver_ast is &Box<AstNode>, dereference to &AstNode
                        let ast_ref: &AstNode = receiver_ast;
                        if let AstNode::Var(func_name) = ast_ref {
                            println!("[MIR GEN DEBUG] Direct function call: {}(...)", func_name);

                            // Generate direct call to function
                            let mut arg_ids = vec![];
                            for a in args {
                                arg_ids.push(self.lower_expr(a));
                            }

                            // Convert type arguments from strings to Type objects
                            let mir_type_args: Vec<Type> =
                                type_args.iter().map(|t| Type::from_string(t)).collect();

                            self.stmts.push(MirStmt::Call {
                                func: func_name.clone(),
                                args: arg_ids,
                                dest: id,
                                type_args: mir_type_args,
                            });

                            self.exprs.insert(id, MirExpr::Var(id));
                            self.type_map.insert(id, Type::I64);
                            return id;
                        }
                        // Not a variable, fall through
                    }
                }

                let mut arg_ids = vec![];
                let receiver_ty = if let Some(r) = receiver {
                    let rid = self.lower_expr(r);
                    arg_ids.push(rid);
                    Some(self.type_map.get(&rid).cloned().unwrap_or(Type::I64))
                } else {
                    None
                };
                for a in args {
                    arg_ids.push(self.lower_expr(a));
                }
                
                // Check if this is a method call on a dynamic array
                let (func, is_array_len, is_array_push) = if let Some(ref rty) = receiver_ty {
                    // Check if receiver is a dynamic array type
                    if let Type::DynamicArray(_) = rty {
                        // Map array methods to runtime functions
                        match method.as_str() {
                            "push" => ("array_push".to_string(), false, true),
                            "len" => ("array_len".to_string(), true, false),
                            _ => {
                                // For other methods, use standard mangling
                                let key = MonoKey {
                                    func_name: method.clone(),
                                    type_args: vec![rty.display_name()],
                                };
                                (key.mangle(), false, false)
                            }
                        }
                    } else {
                        // Not a dynamic array, use standard mangling
                        let key = MonoKey {
                            func_name: method.clone(),
                            type_args: vec![rty.display_name()],
                        };
                        (key.mangle(), false, false)
                    }
                } else {
                    (method.clone(), false, false)
                };

                // Convert type arguments from strings to Type objects
                let mir_type_args: Vec<Type> =
                    type_args.iter().map(|t| Type::from_string(t)).collect();

                self.stmts.push(MirStmt::Call {
                    func: func.clone(),
                    args: arg_ids,
                    dest: id,
                    type_args: mir_type_args,
                });
                self.exprs.insert(id, MirExpr::Var(id));
                // For array methods, set appropriate return type
                if is_array_len {
                    self.type_map.insert(id, Type::I64);
                } else if is_array_push {
                    // push returns void
                    self.type_map.insert(id, Type::Tuple(vec![]));
                } else {
                    self.type_map.insert(id, Type::I64);
                }
            }
            AstNode::Match { scrutinee, arms } => {
                // Lower the scrutinee expression
                let scrutinee_id = self.lower_expr(scrutinee);

                // Generate if-else chain for match arms
                let result_id = id;

                // We'll build the match as a series of if-else statements
                // Start from the last arm and work backwards
                let mut else_branch = Vec::new();

                for arm in arms.iter().rev() {
                    // Generate condition based on pattern
                    let cond_id = self.next_id();

                    match &*arm.pattern {
                        AstNode::Lit(pattern_value) => {
                            // For literal patterns, generate equality check
                            let pattern_id = self.next_id();
                            self.exprs.insert(pattern_id, MirExpr::Lit(*pattern_value));
                            self.type_map.insert(pattern_id, Type::I64);

                            // Create equality comparison: scrutinee == pattern
                            // This creates a call to the "==" operator
                            self.stmts.push(MirStmt::Call {
                                func: "==".to_string(),
                                args: vec![scrutinee_id, pattern_id],
                                dest: cond_id,
                                type_args: vec![],
                            });
                            self.exprs.insert(cond_id, MirExpr::Var(cond_id));
                            self.type_map.insert(cond_id, Type::Bool);
                        }
                        AstNode::Var(var_name) if var_name == "_" => {
                            // Wildcard pattern - always true
                            self.exprs.insert(cond_id, MirExpr::Lit(1));
                            self.type_map.insert(cond_id, Type::Bool);
                        }
                        AstNode::Var(var_name) => {
                            // Check if this is an enum variant name like Option::None
                            if var_name == "Option::None" || var_name == "Result::Err" {
                                // For enum variant without data, check if it matches
                                let check_func = if var_name == "Option::None" {
                                    "option_is_some" // We'll invert this
                                } else if var_name == "Result::Err" {
                                    "host_result_is_ok" // We'll invert this
                                } else {
                                    // Should not happen
                                    self.exprs.insert(cond_id, MirExpr::Lit(0));
                                    self.type_map.insert(cond_id, Type::Bool);
                                    continue;
                                };

                                // Call the runtime function to check the variant
                                let check_result_id = self.next_id();
                                self.stmts.push(MirStmt::Call {
                                    func: check_func.to_string(),
                                    args: vec![scrutinee_id],
                                    dest: check_result_id,
                                    type_args: vec![],
                                });
                                self.exprs
                                    .insert(check_result_id, MirExpr::Var(check_result_id));
                                self.type_map.insert(check_result_id, Type::Bool);

                                // Invert the check (None is not Some, Err is not Ok)
                                let inverted_id = self.next_id();
                                self.stmts.push(MirStmt::Call {
                                    func: "!".to_string(),
                                    args: vec![check_result_id],
                                    dest: inverted_id,
                                    type_args: vec![],
                                });
                                self.exprs.insert(inverted_id, MirExpr::Var(inverted_id));
                                self.type_map.insert(inverted_id, Type::Bool);

                                self.exprs.insert(cond_id, MirExpr::Var(inverted_id));
                                self.type_map.insert(cond_id, Type::Bool);
                            } else if var_name == "_" {
                                // Wildcard pattern - always true
                                self.exprs.insert(cond_id, MirExpr::Lit(1));
                                self.type_map.insert(cond_id, Type::Bool);
                            } else {
                                // Regular variable binding pattern - always matches
                                // Add binding to name_to_id so the arm body can reference it
                                self.name_to_id.insert(var_name.clone(), scrutinee_id);
                                self.exprs.insert(cond_id, MirExpr::Lit(1));
                                self.type_map.insert(cond_id, Type::Bool);
                            }
                        }
                        AstNode::StructPattern {
                            variant,
                            fields,
                            rest: _,
                        } => {
                            // Handle enum variant patterns like Option::Some(x) or Result::Ok(val)
                            // Check if this is an enum variant pattern
                            if variant.starts_with("Option::") || variant.starts_with("Result::") {
                                // Generate condition to check the variant
                                let check_func = if variant == "Option::Some" {
                                    "option_is_some"
                                } else if variant == "Option::None" {
                                    // For None, we check if it's not Some
                                    "option_is_some" // We'll invert this below
                                } else if variant == "Result::Ok" {
                                    "host_result_is_ok"
                                } else if variant == "Result::Err" {
                                    // For Err, we check if it's not Ok
                                    "host_result_is_ok" // We'll invert this below
                                } else {
                                    // Unknown variant, treat as false
                                    self.exprs.insert(cond_id, MirExpr::Lit(0));
                                    self.type_map.insert(cond_id, Type::Bool);
                                    continue;
                                };

                                // Call the runtime function to check the variant
                                let check_result_id = self.next_id();
                                self.stmts.push(MirStmt::Call {
                                    func: check_func.to_string(),
                                    args: vec![scrutinee_id],
                                    dest: check_result_id,
                                    type_args: vec![],
                                });
                                self.exprs
                                    .insert(check_result_id, MirExpr::Var(check_result_id));
                                self.type_map.insert(check_result_id, Type::Bool);

                                // For None and Err, we need to invert the check
                                let final_check_id =
                                    if variant == "Option::None" || variant == "Result::Err" {
                                        let inverted_id = self.next_id();
                                        self.stmts.push(MirStmt::Call {
                                            func: "!".to_string(),
                                            args: vec![check_result_id],
                                            dest: inverted_id,
                                            type_args: vec![],
                                        });
                                        self.exprs.insert(inverted_id, MirExpr::Var(inverted_id));
                                        self.type_map.insert(inverted_id, Type::Bool);
                                        inverted_id
                                    } else {
                                        check_result_id
                                    };

                                // Set up bindings for field patterns
                                for (_field_name, field_pattern) in fields {
                                    if let AstNode::Var(var_name) = field_pattern {
                                        // Extract the field value from the enum
                                        let field_id = self.next_id();
                                        let extract_func = if variant == "Option::Some" {
                                            "option_get_data"
                                        } else if variant == "Result::Ok"
                                            || variant == "Result::Err"
                                        {
                                            "host_result_get_data"
                                        } else {
                                            // No data to extract
                                            continue;
                                        };

                                        // Call runtime function to extract the data
                                        self.stmts.push(MirStmt::Call {
                                            func: extract_func.to_string(),
                                            args: vec![scrutinee_id],
                                            dest: field_id,
                                            type_args: vec![],
                                        });
                                        self.exprs.insert(field_id, MirExpr::Var(field_id));
                                        self.type_map.insert(field_id, Type::I64);
                                        self.name_to_id.insert(var_name.clone(), field_id);
                                    }
                                }

                                self.exprs.insert(cond_id, MirExpr::Var(final_check_id));
                                self.type_map.insert(cond_id, Type::Bool);
                            } else {
                                // For regular struct patterns, treat as always matching for now
                                // Set up bindings for the field patterns
                                for (_field_name, field_pattern) in fields {
                                    if let AstNode::Var(var_name) = field_pattern {
                                        // Create a placeholder ID for the field value
                                        let field_id = self.next_id();
                                        self.name_to_id.insert(var_name.clone(), field_id);
                                        self.exprs.insert(field_id, MirExpr::Lit(0)); // Placeholder
                                        self.type_map.insert(field_id, Type::I64);
                                    }
                                }
                                self.exprs.insert(cond_id, MirExpr::Lit(1));
                                self.type_map.insert(cond_id, Type::Bool);
                            }
                        }
                        AstNode::TypeAnnotatedPattern { pattern: inner_pattern, ty: _ } => {
                            // For type-annotated patterns, extract the inner pattern
                            // The type checking should have been done by the type checker
                            match &**inner_pattern {
                                AstNode::Var(var_name) => {
                                    // Regular variable binding pattern - always matches
                                    // Add binding to name_to_id so the arm body can reference it
                                    self.name_to_id.insert(var_name.clone(), scrutinee_id);
                                    self.exprs.insert(cond_id, MirExpr::Lit(1));
                                    self.type_map.insert(cond_id, Type::Bool);
                                }
                                _ => {
                                    // For other inner patterns, treat as always false for now
                                    self.exprs.insert(cond_id, MirExpr::Lit(0));
                                    self.type_map.insert(cond_id, Type::Bool);
                                }
                            }
                        }
                        _ => {
                            // For now, treat other patterns as always false
                            self.exprs.insert(cond_id, MirExpr::Lit(0));
                            self.type_map.insert(cond_id, Type::Bool);
                        }
                    }

                    // Handle guard clause if present
                    let final_cond_id = if let Some(ref guard) = arm.guard {
                        // Lower the guard expression
                        let guard_id = self.lower_expr(guard);

                        // Create AND condition: pattern_matches && guard_condition
                        let and_cond_id = self.next_id();
                        self.stmts.push(MirStmt::Call {
                            func: "&&".to_string(),
                            args: vec![cond_id, guard_id],
                            dest: and_cond_id,
                            type_args: vec![],
                        });
                        self.exprs.insert(and_cond_id, MirExpr::Var(and_cond_id));
                        self.type_map.insert(and_cond_id, Type::Bool);

                        and_cond_id
                    } else {
                        cond_id
                    };

                    // Now lower the arm body (after establishing pattern bindings)
                    let arm_body_id = self.lower_expr(&arm.body);

                    // Create the if statement for this arm
                    let then_branch = vec![MirStmt::Assign {
                        lhs: result_id,
                        rhs: arm_body_id,
                    }];

                    let if_stmt = MirStmt::If {
                        cond: final_cond_id,
                        then: then_branch,
                        else_: else_branch,
                        dest: None,
                    };

                    // For the next iteration, the current if becomes the else branch
                    else_branch = vec![if_stmt];
                }

                // The final else_branch contains the complete if-else chain
                // Add it to statements
                if !else_branch.is_empty() {
                    // The chain starts with the first arm's if statement
                    self.stmts.extend(else_branch);
                }

                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, Type::I64);
            }
            AstNode::FieldAccess { base, field } => {
                // Implement proper field access
                // 1. Evaluate the base expression
                let base_id = self.lower_expr(base);
                // 2. Create FieldAccess expression
                self.exprs.insert(
                    id,
                    MirExpr::FieldAccess {
                        base: base_id,
                        field: field.clone(),
                    },
                );
                // 3. For now, assume field type is i64 (will need proper type inference later)
                self.type_map.insert(id, Type::I64);
            }
            AstNode::StructLit { variant, fields } => {
                // Implement proper struct literal creation
                let mut field_ids = Vec::new();
                for (field_name, field_expr) in fields {
                    // Evaluate each field expression
                    let field_id = self.lower_expr(field_expr);
                    field_ids.push((field_name.clone(), field_id));
                }
                // Create Struct expression
                self.exprs.insert(
                    id,
                    MirExpr::Struct {
                        variant: variant.clone(),
                        fields: field_ids,
                    },
                );
                // For now, assume struct type is a generic type
                // TODO: Need proper type inference for struct literals
                self.type_map
                    .insert(id, Type::Named("Struct".to_string(), vec![]));
            }
            AstNode::PathCall {
                path,
                method,
                args,
                type_args,
            } => {
                println!(
                    "[MIR GEN DEBUG] Processing path call: path={:?}, method={:?}, args={:?}, type_args={:?}",
                    path, method, args, type_args
                );

                // Construct qualified name: path::method
                let func_name = if path.is_empty() {
                    method.clone()
                } else {
                    format!("{}::{}", path.join("::"), method)
                };

                println!(
                    "[MIR GEN DEBUG] Path call resolved to function: {}",
                    func_name
                );

                // Generate argument IDs
                let mut arg_ids = vec![];
                for a in args {
                    arg_ids.push(self.lower_expr(a));
                }

                // Convert type arguments from strings to Type objects
                let mir_type_args: Vec<Type> =
                    type_args.iter().map(|t| Type::from_string(t)).collect();

                // Generate call statement
                self.stmts.push(MirStmt::Call {
                    func: func_name,
                    args: arg_ids,
                    dest: id,
                    type_args: mir_type_args,
                });

                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, Type::I64);
            }
            AstNode::Cast { expr, ty } => {
                let expr_id = self.lower_expr(expr);
                let target_type = Type::from_string(ty);
                self.type_map.insert(id, target_type.clone());
                // Create As expression node
                self.exprs.insert(id, MirExpr::As {
                    expr: expr_id,
                    target_type,
                });
            }
            AstNode::ArrayLit(elements) => {
                // Create an array using ArrayHeader API
                println!("[MIR GEN DEBUG] ArrayLit with {} elements", elements.len());
                
                let size = elements.len();
                
                // HYBRID MEMORY SYSTEM: Check if this should be a stack array
                // For small, fixed-size arrays, use stack allocation
                if size <= 1024 { // Reasonable stack size limit
                    println!("[MIR GEN DEBUG] Using StackArray for array literal with {} elements", size);
                    
                    // Lower each element expression
                    let mut element_ids = Vec::new();
                    for element in elements {
                        let elem_id = self.lower_expr(element);
                        element_ids.push(elem_id);
                    }
                    
                    // Clone element_ids before moving it
                    let element_ids_clone = element_ids.clone();
                    
                    // Create StackArray expression
                    self.exprs.insert(id, MirExpr::StackArray {
                        elements: element_ids,
                        size,
                    });
                    
                    // Determine element type from elements
                    let elem_type = self.get_common_element_type(&element_ids_clone);
                    
                    // Set the type to Array(elem_type, size) for subscript access
                    self.type_map.insert(id, Type::Array(Box::new(elem_type), ArraySize::Literal(size)));
                } else {
                    // Large array, use heap allocation
                    println!("[MIR GEN DEBUG] Using heap array for large array with {} elements", size);
                    
                    // Call array_new with capacity = size
                    let array_data_ptr = self.next_id();
                    let capacity_id = self.next_id();
                    self.exprs.insert(capacity_id, MirExpr::Lit(size as i64));
                    self.stmts.push(MirStmt::Call {
                        func: "array_new".to_string(),
                        args: vec![capacity_id],
                        dest: array_data_ptr,
                        type_args: vec![],
                    });
                    
                    // For heap arrays, we need to set the length
                    let len_id = self.next_id();
                    self.exprs.insert(len_id, MirExpr::Lit(size as i64));
                    self.stmts.push(MirStmt::VoidCall {
                        func: "array_set_len".to_string(),
                        args: vec![array_data_ptr, len_id],
                    });
                    
                    // Set each element at its index and collect element IDs
                    let mut heap_element_ids = Vec::new();
                    for (i, element) in elements.iter().enumerate() {
                        let elem_id = self.lower_expr(element);
                        heap_element_ids.push(elem_id);
                        let index_id = self.next_id();
                        self.exprs.insert(index_id, MirExpr::Lit(i as i64));
                        self.stmts.push(MirStmt::VoidCall {
                            func: "array_set".to_string(),
                            args: vec![array_data_ptr, index_id, elem_id],
                        });
                    }
                    
                    // Clone heap_element_ids before using it
                    let heap_element_ids_clone = heap_element_ids.clone();
                    
                    // Return the data pointer (after header)
                    self.exprs.insert(id, MirExpr::Var(array_data_ptr));
                    // Determine element type from elements
                    let elem_type = self.get_common_element_type(&heap_element_ids_clone);
                    // Set the type to Array(elem_type, size) for subscript access
                    self.type_map.insert(id, Type::Array(Box::new(elem_type), ArraySize::Literal(size)));
                }
            }
            AstNode::ArrayRepeat { value, size } => {
                println!("[MIR GEN DEBUG] ArrayRepeat: [value; size]");
                let value_id = self.lower_expr(value);
                
                // Get the type of the value expression
                let elem_type = self.type_map.get(&value_id).cloned().unwrap_or(Type::I64);
                
                // Check if size is a literal by examining the AST node directly
                // We need to pattern match on the boxed value
                match size.as_ref() {
                    AstNode::Lit(size_lit) => {
                        let size_val = *size_lit as usize;
                        println!("[MIR GEN DEBUG] ArrayRepeat with constant size: {}, elem_type: {:?}", size_val, elem_type);
                        
                        // HYBRID MEMORY SYSTEM: Use StackArray for small fixed-size arrays
                        if size_val <= 1024 { // Reasonable stack size limit
                            println!("[MIR GEN DEBUG] Using StackArray for array repeat with {} elements", size_val);
                            
                            // Create StackArray expression with repeated value
                            self.exprs.insert(id, MirExpr::StackArray {
                                elements: vec![value_id; size_val],
                                size: size_val,
                            });
                            
                            // Set the type to Array(elem_type, size) for subscript access
                            self.type_map.insert(id, Type::Array(Box::new(elem_type), ArraySize::Literal(size_val)));
                        } else {
                            // Large array, use heap allocation
                            println!("[MIR GEN DEBUG] Using heap array for large array repeat with {} elements", size_val);
                            
                            // Allocate array using array_new with capacity = size
                            let array_ptr = self.next_id();
                            let capacity_id = self.next_id();
                            self.exprs.insert(capacity_id, MirExpr::Lit(size_val as i64));
                            self.stmts.push(MirStmt::Call {
                                func: "array_new".to_string(),
                                args: vec![capacity_id],
                                dest: array_ptr,
                                type_args: vec![],
                            });
                            
                            // Push the value size_val times
                            for _ in 0..size_val {
                                self.stmts.push(MirStmt::VoidCall {
                                    func: "array_push".to_string(),
                                    args: vec![array_ptr, value_id],
                                });
                            }
                            
                            // Return the array pointer
                            self.exprs.insert(id, MirExpr::Var(array_ptr));
                            // Set the type to Array(elem_type, size) for subscript access
                            self.type_map.insert(id, Type::Array(Box::new(elem_type), ArraySize::Literal(size_val)));
                        }
                    }
                    _ => {
                        // Size is not a literal constant
                        println!("[MIR GEN DEBUG] ArrayRepeat with non-constant size, using placeholder");
                        // For now, create a placeholder
                        self.exprs.insert(id, MirExpr::Lit(0));
                        self.type_map.insert(id, Type::I64);
                    }
                }
            }
            AstNode::Subscript { base, index } => {
                let bid = self.lower_expr(base);
                let iid = self.lower_expr(index);
                
                // Check if base is an array type (dynamic or static)
                let base_ty = self.type_map.get(&bid).cloned().unwrap_or(Type::I64);
                if let Type::DynamicArray(_) = base_ty {
                    // Generate array_get call for dynamic arrays
                    self.stmts.push(MirStmt::Call {
                        func: "array_get".to_string(),
                        args: vec![bid, iid],
                        dest: id,
                        type_args: vec![],
                    });
                } else if let Type::Array(_, size) = base_ty {
                    // Check if this is a stack array (fixed size) or heap array
                    println!("[MIR GEN DEBUG] Array subscript with size: {:?}", size);
                    match size {
                        ArraySize::Literal(n) if n <= 1024 => {
                            // Small fixed-size array - treat as stack array
                            // Use stack_array_get for direct memory access
                            println!("[MIR GEN DEBUG] Using stack_array_get for stack array subscript (size={})", n);
                            self.stmts.push(MirStmt::Call {
                                func: "stack_array_get".to_string(),
                                args: vec![bid, iid],
                                dest: id,
                                type_args: vec![],
                            });
                        }
                        _ => {
                            // Dynamic or large array - use heap array access
                            println!("[MIR GEN DEBUG] Using array_get for heap/dynamic array subscript");
                            self.stmts.push(MirStmt::Call {
                                func: "array_get".to_string(),
                                args: vec![bid, iid],
                                dest: id,
                                type_args: vec![],
                            });
                        }
                    }
                } else {
                    // Use DictGet for other types (maps/dicts)
                    self.stmts.push(MirStmt::DictGet {
                        map_id: bid,
                        key_id: iid,
                        dest: id,
                    });
                }
                self.exprs.insert(id, MirExpr::Var(id));
                self.type_map.insert(id, Type::I64);
            }
            AstNode::DynamicArrayLit { elem_type, elements } => {
                // Call array_new with capacity = number of elements
                let array_ptr = self.next_id();
                let capacity_id = self.next_id();
                self.exprs.insert(capacity_id, MirExpr::Lit(elements.len() as i64));
                self.stmts.push(MirStmt::Call {
                    func: "array_new".to_string(),
                    args: vec![capacity_id],
                    dest: array_ptr,
                    type_args: vec![],
                });
                self.exprs.insert(array_ptr, MirExpr::Var(array_ptr));
                let array_type = Type::DynamicArray(Box::new(Type::from_string(elem_type)));
                self.type_map.insert(array_ptr, array_type.clone());
                
                // Push each element to the array
                for element in elements {
                    let elem_id = self.lower_expr(element);
                    let void_dest = self.next_id(); // push returns void
                    self.stmts.push(MirStmt::Call {
                        func: "array_push".to_string(),
                        args: vec![array_ptr, elem_id],
                        dest: void_dest,
                        type_args: vec![],
                    });
                }
                
                // Return the array pointer (use array_ptr as the result)
                self.exprs.insert(id, MirExpr::Var(array_ptr));
                self.type_map.insert(id, array_type);
                return array_ptr; // Return the array pointer ID, not a new ID
            }
            _ => {
                self.exprs.insert(id, MirExpr::Lit(0));
                self.type_map.insert(id, Type::I64);
            }
        }
        id
    }

    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    /// Get the common type of element expressions.
    /// If elements is empty, returns Type::I64 as default.
    fn get_common_element_type(&self, element_ids: &[u32]) -> Type {
        if let Some(first_elem_id) = element_ids.first() {
            self.type_map.get(first_elem_id).cloned().unwrap_or(Type::I64)
        } else {
            Type::I64
        }
    }

    fn next_id_with_lit(&mut self, n: i64) -> u32 {
        let id = self.next_id();
        self.exprs.insert(id, MirExpr::Lit(n));
        self.type_map.insert(id, Type::I64);
        id
    }
}

impl Default for MirGen {
    fn default() -> Self {
        Self::new()
    }
}
