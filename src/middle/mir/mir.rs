// src/middle/mir/mir.rs
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct Mir {
    pub name: Option<String>,
    pub param_indices: Vec<(String, u32)>,
    pub stmts: Vec<MirStmt>,
    pub exprs: HashMap<u32, MirExpr>,
    pub ctfe_consts: HashMap<u32, i64>,
    pub type_map: HashMap<u32, String>,
}

#[derive(Debug, Clone)]
pub enum MirStmt {
    Assign {
        lhs: u32,
        rhs: u32,
    },
    Call {
        func: String,
        args: Vec<u32>,
        dest: u32,
        type_args: Vec<String>,
    },
    VoidCall {
        func: String,
        args: Vec<u32>,
    },
    Return {
        val: u32,
    },
    SemiringFold {
        op: SemiringOp,
        values: Vec<u32>,
        result: u32,
    },
    ParamInit {
        param_id: u32,
        arg_index: u32,
    },
    Consume {
        id: u32,
    },
    If {
        cond: u32,
        then: Vec<MirStmt>,
        else_: Vec<MirStmt>,
    },
    TryProp {
        expr_id: u32,
        ok_dest: u32,
        err_dest: u32,
    },
    DictInsert {
        map_id: u32,
        key_id: u32,
        val_id: u32,
    },
    DictGet {
        map_id: u32,
        key_id: u32,
        dest: u32,
    },
    MapNew {
        dest: u32,
    },
}

#[derive(Debug, Clone)]
pub enum MirExpr {
    Var(u32),
    Lit(i64),
    StringLit(String),
    FString(Vec<u32>),
    ConstEval(i64),
    TimingOwned(u32),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SemiringOp {
    Add,
    Mul,
}
