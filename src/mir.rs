// src/mir.rs
use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SemiringOp { Add, Mul }

#[derive(Debug, Clone)]
pub struct Mir {
    pub stmts: Vec<MirStmt>,
    pub locals: HashMap<String, u32>,
    pub ctfe_consts: HashMap<u32, i64>,
}

#[derive(Debug, Clone)]
pub enum MirStmt {
    Assign { lhs: u32, rhs: MirExpr },
    Call { func: String, args: Vec<u32>, dest: u32 },
    Return { val: u32 },
    Defer { stmt: Box<MirStmt> },
    SemiringFold { op: SemiringOp, values: Vec<u32>, result: u32 },
    Spawn { actor: u32, dest: u32 },
    Send { channel: u32, value: u32 },
    Recv { channel: u32, dest: u32 },
    Await { future: u32, dest: u32 },
}

#[derive(Debug, Clone)]
pub enum MirExpr {
    Var(u32),
    Lit(i64),
    MethodCall { recv: u32, method: String, args: Vec<u32> },
    ConstEval(i64),
}
