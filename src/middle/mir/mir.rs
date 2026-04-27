// src/middle/mir/mir.rs
use crate::middle::types::Type;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Mir {
    pub name: Option<String>,
    pub param_indices: Vec<(String, u32)>,
    pub stmts: Vec<MirStmt>,
    pub exprs: HashMap<u32, MirExpr>,
    pub ctfe_consts: HashMap<u32, i64>,
    pub type_map: HashMap<u32, Type>,
    pub global_consts: HashMap<String, crate::middle::ctfe::value::ConstValue>,
}

impl Default for Mir {
    fn default() -> Self {
        Self {
            name: None,
            param_indices: Vec::new(),
            stmts: Vec::new(),
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
            type_map: HashMap::new(),
            global_consts: HashMap::new(),
        }
    }
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
        type_args: Vec<Type>,
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
        dest: Option<u32>, // Destination for if expression value (None for statement if)
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
    // Struct creation
    StructNew {
        variant: String,
        fields: Vec<(String, u32)>,
        dest: u32,
    },
    // For loop
    For {
        iterator: u32,   // Iterator expression
        pattern: String, // Variable name to bind to
        var_id: u32,     // Variable ID for the loop variable
        body: Vec<MirStmt>,
    },
    // While loop
    While {
        cond: u32,       // Condition expression
        body: Vec<MirStmt>,
    },
    // Store through pointer: *addr = value
    // pointee_width: 1 for *mut u8, 8 for *mut u64/i64, etc.
    Store {
        addr_id: u32,
        val_id: u32,
        pointee_width: u8,
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
    // Struct support
    Struct {
        variant: String,
        fields: Vec<(String, u32)>,
    },
    FieldAccess {
        base: u32,
        field: String,
    },
    // Type conversion expression
    As {
        expr: u32,
        target_type: Type,
    },
    // Range expression for for loops
    Range {
        start: u32,
        end: u32,
    },
    // Pointer dereference: load through a pointer with explicit element width
    // pointee_width: 1 for *mut u8, 8 for *mut u64/i64, etc.
    Deref {
        addr_id: u32,
        pointee_width: u8,
    },
    // Binary operation
    BinaryOp {
        op: String,
        left: u32,
        right: u32,
    },
    // Stack array literal (for hybrid memory system)
    StackArray {
        elements: Vec<u32>,
        size: usize,
    },
    // Semiring fold (arithmetic) — evaluated inline so while-loop
    // conditions can compute them directly instead of loading from
    // a slot written by a side-effect statement inside the body.
    SemiringFold {
        op: SemiringOp,
        values: Vec<u32>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SemiringOp {
    Add,
    Mul,
}
