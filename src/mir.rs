// src/mir.rs
//! Mid-level Intermediate Representation (MIR) for Zeta.
//! Bridges the Abstract Syntax Tree (AST) to LLVM code generation.
//! Defines statements for control flow, assignments, and calls.
//! Includes expressions for literals, variables, and operations.
use crate::ast::AstNode;
use crate::resolver::{Resolver, Type};
use std::collections::HashMap;
/// Operations for semiring-based algebraic optimizations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemiringOp {
    /// Addition operation.
    Add,
    /// Multiplication operation.
    Mul,
}
/// Represents a MIR function or block.
#[derive(Debug, Clone)]
pub struct Mir {
    /// Sequence of MIR statements.
    pub stmts: Vec<MirStmt>,
    /// Mapping from local names to IDs.
    pub locals: HashMap<String, u32>,
    /// Mapping from expression IDs to expressions.
    pub exprs: HashMap<u32, MirExpr>,
    /// Optional function name.
    pub name: Option<String>,
    /// Parameter indices as (name, arg_index).
    pub param_indices: Vec<(String, usize)>,
}
/// MIR statement variants.
#[derive(Debug, Clone)]
pub enum MirStmt {
    /// Assigns an expression to a local.
    Assign { lhs: u32, rhs: MirExpr },
    /// Function call with return value.
    Call {
        func: String,
        args: Vec<u32>,
        dest: u32,
        type_args: Vec<String>,
    },
    /// Void function call.
    VoidCall { func: String, args: Vec<u32> },
    /// Return statement with value.
    Return { val: u32 },
    /// Folds values using a semiring operation.
    SemiringFold {
        op: SemiringOp,
        values: Vec<u32>,
        result: u32,
    },
    /// Initializes a parameter from an argument.
    ParamInit { param_id: u32, arg_index: usize },
    /// Marks a local as consumed for affine types.
    Consume { id: u32 },
    /// Conditional branch statement.
    If {
        cond: u32,
        then: Vec<MirStmt>,
        else_: Vec<MirStmt>,
    },
}
/// MIR expression variants.
#[derive(Debug, Clone)]
pub enum MirExpr {
    /// Reference to a local variable by ID.
    Var(u32),
    /// Integer literal.
    Lit(i64),
    /// String literal.
    StringLit(String),
    /// Formatted string parts.
    FString(Vec<u32>),
    /// Constant-evaluated value.
    ConstEval(i64),
    /// Timing-owned wrapper for constant-time access.
    TimingOwned(u32),
}
/// Internal info for deferred calls.
#[derive(Debug, Clone)]
struct DeferInfo {
    func: String,
    args: Vec<u32>,
}
/// Generator for MIR from AST nodes.
pub struct MirGen<'a> {
    next_id: u32,
    locals: HashMap<String, u32>,
    defers: Vec<DeferInfo>,
    param_indices: Vec<(String, usize)>,
    #[allow(dead_code)]
    exprs: HashMap<u32, MirExpr>,
    resolver: &'a Resolver,
}
impl<'a> MirGen<'a> {
    /// Creates a new MIR generator.
    pub fn new(resolver: &'a Resolver) -> Self {
        Self {
            next_id: 0,
            locals: HashMap::new(),
            defers: vec![],
            param_indices: vec![],
            exprs: HashMap::new(),
            resolver,
        }
    }
    /// Generates MIR for an AST node, typically a function definition.
    pub fn gen_mir(&mut self, ast: &AstNode) -> Mir {
        let mut stmts = vec![];
        let mut exprs = HashMap::new();
        let name = if let AstNode::FuncDef { name, .. } = ast {
            Some(name.clone())
        } else {
            None
        };
        let ret_expr = if let AstNode::FuncDef { ret_expr, .. } = ast {
            ret_expr.clone()
        } else {
            None
        };
        if let AstNode::FuncDef {
            params, body, ret, ..
        } = ast
        {
            // Alloc param locals and track for init
            self.param_indices.clear();
            for (i, (pname, _)) in params.iter().enumerate() {
                self.alloc_local(pname);
                self.param_indices.push((pname.clone(), i));
            }
            // Add ParamInit stmts at entry
            for (pname, arg_idx) in &self.param_indices {
                if let Some(param_id) = self.locals.get(pname) {
                    stmts.push(MirStmt::ParamInit {
                        param_id: *param_id,
                        arg_index: *arg_idx,
                    });
                }
            }
            for stmt in body {
                self.gen_stmt_full(stmt, &mut stmts, &mut exprs);
            }
            // Insert defers before return in reverse order
            for info in self.defers.iter().rev() {
                stmts.push(MirStmt::VoidCall {
                    func: info.func.clone(),
                    args: info.args.clone(),
                });
            }
            if let Some(ret_expr) = ret_expr {
                let ret_val = self.gen_expr(&*ret_expr, &mut exprs, &mut stmts);
                let ret_id = self.materialize(ret_val, &mut exprs, &mut stmts);
                stmts.push(MirStmt::Return { val: ret_id });
            } else if let Some(last) = body.last() {
                if let AstNode::Assign(_, expr) = last {
                    let ret_val = self.gen_expr(expr, &mut exprs, &mut stmts);
                    let ret_id = self.materialize(ret_val, &mut exprs, &mut stmts);
                    stmts.push(MirStmt::Return { val: ret_id });
                } else if !matches!(last, AstNode::Return(_)) {
                    let last_expr = self.gen_expr(last, &mut exprs, &mut stmts);
                    let last_id = self.materialize(last_expr, &mut exprs, &mut stmts);
                    let is_error_prop = ret.starts_with("Result<");
                    if is_error_prop {
                        let wrap_id = self.next_id();
                        stmts.push(MirStmt::Call {
                            func: "result_make_ok".to_string(),
                            args: vec![last_id],
                            dest: wrap_id,
                            type_args: vec![],
                        });
                        stmts.push(MirStmt::Return { val: wrap_id });
                    } else {
                        stmts.push(MirStmt::Return { val: last_id });
                    }
                }
            }
        }
        Mir {
            stmts,
            locals: self.locals.clone(),
            exprs,
            name,
            param_indices: self.param_indices.clone(),
        }
    }
    /// Generates a statement and adds to output.
    fn gen_stmt_full(
        &mut self,
        stmt: &AstNode,
        out: &mut Vec<MirStmt>,
        exprs: &mut HashMap<u32, MirExpr>,
    ) {
        match stmt {
            AstNode::Assign(lhs, rhs) => {
                let rhs_expr = self.gen_expr(rhs.as_ref(), exprs, out);
                let rhs_id = self.materialize(rhs_expr, exprs, out);
                if let AstNode::Var(name) = &**lhs {
                    let lhs_id = self.lookup_or_alloc(name);
                    out.push(MirStmt::Assign {
                        lhs: lhs_id,
                        rhs: MirExpr::Var(rhs_id),
                    });
                } else if let AstNode::Subscript { base, index } = &**lhs {
                    let base_ty = self.resolver.infer_type(base.as_ref());
                    let (k_ty_str, _) = if let Type::Named { params, .. } = &base_ty {
                        if params.len() == 2 {
                            (params[0].to_string(), params[1].to_string())
                        } else {
                            ("i64".to_string(), "i64".to_string())
                        }
                    } else {
                        ("i64".to_string(), "i64".to_string())
                    };
                    let insert_fn = format!("map_insert_{}", k_ty_str);
                    let base_expr = self.gen_expr(base.as_ref(), exprs, out);
                    let base_id = self.materialize(base_expr, exprs, out);
                    let index_expr = self.gen_expr(index.as_ref(), exprs, out);
                    let index_id = self.materialize(index_expr, exprs, out);
                    out.push(MirStmt::VoidCall {
                        func: insert_fn,
                        args: vec![base_id, index_id, rhs_id],
                    });
                }
            }
            AstNode::Call {
                receiver,
                method,
                args,
                type_args,
                ..
            } => {
                let mut arg_ids = vec![];
                if let Some(rec) = receiver.as_ref() {
                    let rec_expr = self.gen_expr(rec, exprs, out);
                    let rec_id = self.materialize(rec_expr, exprs, out);
                    arg_ids.push(rec_id);
                }
                for arg in args {
                    let arg_expr = self.gen_expr(arg, exprs, out);
                    let arg_id = self.materialize(arg_expr, exprs, out);
                    arg_ids.push(arg_id);
                }
                let func = method.clone();
                let dest = self.next_id();
                out.push(MirStmt::Call {
                    func,
                    args: arg_ids,
                    dest,
                    type_args: type_args.clone(),
                });
            }
            AstNode::Return(inner) => {
                let val_expr = self.gen_expr(inner.as_ref(), exprs, out);
                let val_id = self.materialize(val_expr, exprs, out);
                out.push(MirStmt::Return { val: val_id });
            }
            AstNode::Defer(inner) => {
                if let AstNode::Call {
                    method, args, ..
                } = inner.as_ref() {
                    let arg_ids = args
                        .iter()
                        .map(|a| {
                            let a_expr = self.gen_expr(a, exprs, out);
                            self.materialize(a_expr, exprs, out)
                        })
                        .collect();
                    self.defers.push(DeferInfo {
                        func: method.clone(),
                        args: arg_ids,
                    });
                }
            }
            AstNode::TryProp { expr } => {
                let expr_id = self.gen_expr(expr.as_ref(), exprs, out);
                let id = self.materialize(expr_id, exprs, out);
                let is_ok_dest = self.next_id();
                out.push(MirStmt::Call {
                    func: "result_is_ok".to_string(),
                    args: vec![id],
                    dest: is_ok_dest,
                    type_args: vec![],
                });
                let then = vec![];
                let mut else_ = vec![MirStmt::Call {
                    func: "result_get_data".to_string(),
                    args: vec![id],
                    dest: self.next_id(),
                    type_args: vec![],
                }];
                else_.push(MirStmt::Return { val: self.locals[&"err".to_string()] }); // Assume err local
                out.push(MirStmt::If {
                    cond: is_ok_dest,
                    then,
                    else_,
                });
                let data_dest = self.next_id();
                out.push(MirStmt::Call {
                    func: "result_get_data".to_string(),
                    args: vec![id],
                    dest: data_dest,
                    type_args: vec![],
                });
            }
            AstNode::If { cond, then, else_ } => {
                let cond_expr = self.gen_expr(cond.as_ref(), exprs, out);
                let cond_id = self.materialize(cond_expr, exprs, out);
                let mut then_stmts = vec![];
                for s in then {
                    self.gen_stmt_full(s, &mut then_stmts, exprs);
                }
                let mut else_stmts = vec![];
                for s in else_ {
                    self.gen_stmt_full(s, &mut else_stmts, exprs);
                }
                out.push(MirStmt::If {
                    cond: cond_id,
                    then: then_stmts,
                    else_: else_stmts,
                });
            }
            _ => {}
        }
    }
    /// Generates MIR for an expression.
    fn gen_expr(
        &mut self,
        expr: &AstNode,
        exprs: &mut HashMap<u32, MirExpr>,
        out: &mut Vec<MirStmt>,
    ) -> MirExpr {
        match expr {
            AstNode::Var(name) => MirExpr::Var(self.lookup_or_alloc(name)),
            AstNode::Lit(n) => MirExpr::Lit(*n),
            AstNode::StringLit(s) => MirExpr::StringLit(s.clone()),
            AstNode::FString(parts) => {
                let part_ids = parts
                    .iter()
                    .map(|p| {
                        let p_expr = self.gen_expr(p, exprs, out);
                        self.materialize(p_expr, exprs, out)
                    })
                    .collect();
                MirExpr::FString(part_ids)
            }
            AstNode::BinaryOp { op, left, right } => {
                let l_expr = self.gen_expr(left.as_ref(), exprs, out);
                let l_id = self.materialize(l_expr, exprs, out);
                let r_expr = self.gen_expr(right.as_ref(), exprs, out);
                let r_id = self.materialize(r_expr, exprs, out);
                let func = match op.as_str() {
                    "+" => "add".to_string(),
                    "*" => "mul".to_string(),
                    _ => "unknown".to_string(),
                };
                let dest = self.next_id();
                out.push(MirStmt::Call {
                    func,
                    args: vec![l_id, r_id],
                    dest,
                    type_args: vec![],
                });
                MirExpr::Var(dest)
            }
            AstNode::TimingOwned { inner, .. } => {
                let inner_expr = self.gen_expr(inner.as_ref(), exprs, out);
                let inner_id = self.materialize(inner_expr, exprs, out);
                MirExpr::TimingOwned(inner_id)
            }
            AstNode::Call { receiver, method, args, type_args, .. } => {
                let mut arg_ids = vec![];
                if let Some(rec) = receiver.as_ref() {
                    let rec_expr = self.gen_expr(rec, exprs, out);
                    let rec_id = self.materialize(rec_expr, exprs, out);
                    arg_ids.push(rec_id);
                }
                for arg in args {
                    let arg_expr = self.gen_expr(arg, exprs, out);
                    let arg_id = self.materialize(arg_expr, exprs, out);
                    arg_ids.push(arg_id);
                }
                let func = method.clone();
                let dest = self.next_id();
                out.push(MirStmt::Call {
                    func,
                    args: arg_ids,
                    dest,
                    type_args: type_args.clone(),
                });
                MirExpr::Var(dest)
            }
            AstNode::DictLit { entries } => {
                let dict_ty = self.resolver.infer_type(expr);
                let (k_ty_str, v_ty_str) = if let Type::Named { params, .. } = &dict_ty {
                    if params.len() == 2 {
                        (params[0].to_string(), params[1].to_string())
                    } else {
                        ("i64".to_string(), "i64".to_string())
                    }
                } else {
                    ("i64".to_string(), "i64".to_string())
                };
                let new_fn = format!("map_new_{}_{}", k_ty_str, v_ty_str);
                let insert_fn = format!("map_insert_{}", k_ty_str);
                let dest = self.next_id();
                out.push(MirStmt::Call {
                    func: new_fn,
                    args: vec![],
                    dest,
                    type_args: vec![],
                });
                for (k, v) in entries {
                    let k_expr = self.gen_expr(k, exprs, out);
                    let k_id = self.materialize(k_expr, exprs, out);
                    let v_expr = self.gen_expr(v, exprs, out);
                    let v_id = self.materialize(v_expr, exprs, out);
                    out.push(MirStmt::VoidCall {
                        func: insert_fn.clone(),
                        args: vec![dest, k_id, v_id],
                    });
                }
                MirExpr::Var(dest)
            }
            AstNode::Subscript { base, index } => {
                let base_ty = self.resolver.infer_type(base.as_ref());
                let (k_ty_str, _) = if let Type::Named { params, .. } = &base_ty {
                    if params.len() == 2 {
                        (params[0].to_string(), params[1].to_string())
                    } else {
                        ("i64".to_string(), "i64".to_string())
                    }
                } else {
                    ("i64".to_string(), "i64".to_string())
                };
                let get_fn = format!("map_get_{}", k_ty_str);
                let base_expr = self.gen_expr(base.as_ref(), exprs, out);
                let base_id = self.materialize(base_expr, exprs, out);
                let index_expr = self.gen_expr(index.as_ref(), exprs, out);
                let index_id = self.materialize(index_expr, exprs, out);
                let dest = self.next_id();
                out.push(MirStmt::Call {
                    func: get_fn,
                    args: vec![base_id, index_id],
                    dest,
                    type_args: vec![],
                });
                MirExpr::Var(dest)
            }
            _ => MirExpr::Lit(0),
        }
    }
    /// Materializes an expression to an ID without statements.
    #[allow(dead_code)]
    fn materialize_inner(&mut self, expr: &MirExpr, exprs: &mut HashMap<u32, MirExpr>) -> u32 {
        match expr {
            MirExpr::Var(id) => *id,
            _ => {
                let id = self.next_id();
                exprs.insert(id, expr.clone());
                id
            }
        }
    }
    /// Materializes an expression to an ID, adding statements if needed.
    fn materialize(
        &mut self,
        expr: MirExpr,
        exprs: &mut HashMap<u32, MirExpr>,
        out: &mut Vec<MirStmt>,
    ) -> u32 {
        match expr {
            MirExpr::Var(id) => id,
            MirExpr::TimingOwned(inner_id) => {
                let id = self.next_id();
                exprs.insert(id, MirExpr::TimingOwned(inner_id));
                id
            }
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
    /// Allocates a new local ID for a name.
    fn alloc_local(&mut self, name: &str) -> u32 {
        let id = self.next_id();
        self.locals.insert(name.to_string(), id);
        id
    }
    /// Looks up or allocates a local ID for a name.
    fn lookup_or_alloc(&mut self, name: &str) -> u32 {
        let key = name.to_string();
        if !self.locals.contains_key(&key) {
            let id = self.next_id();
            self.locals.insert(key, id);
            id
        } else {
            *self.locals.get(&key).unwrap()
        }
    }
    /// Generates the next unique ID.
    fn next_id(&mut self) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}
impl Mir {
    /// Optimizes MIR by constant-folding semiring operations.
    pub fn optimize_ctfe(&mut self) {
        let mut i = 0;
        while i < self.stmts.len() {
            if let MirStmt::Call {
                func, args, dest, ..
            } = &mut self.stmts[i]
            {
                // If call to semiring op with all const args, fold
                if func == "add" || func == "mul" {
                    let op = if func == "add" {
                        SemiringOp::Add
                    } else {
                        SemiringOp::Mul
                    };
                    let all_const = args
                        .iter()
                        .all(|&id| matches!(self.exprs.get(&id), Some(MirExpr::Lit(_))));
                    if all_const {
                        let values: Vec<i64> = args
                            .iter()
                            .map(|&id| {
                                if let MirExpr::Lit(n) = self.exprs[&id] {
                                    n
                                } else {
                                    0
                                }
                            })
                            .collect();
                        let folded = match op {
                            SemiringOp::Add => values.iter().sum(),
                            SemiringOp::Mul => values.iter().product(),
                        };
                        self.exprs.insert(*dest, MirExpr::ConstEval(folded));
                        // Replace stmt with assign from ConstEval
                        self.stmts[i] = MirStmt::Assign {
                            lhs: *dest,
                            rhs: MirExpr::ConstEval(folded),
                        };
                    }
                }
            }
            i += 1;
        }
    }
}
