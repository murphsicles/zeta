// src/ast.rs
//! Abstract Syntax Tree for Zeta.
//! Defines nodes for expressions, statements, definitions, and algebraic constructs.
//! Extended for self-host: enums, structs, strings, path calls.
//! Added: generics in FuncDef/Method, structural flag in Call for hybrid dispatch.
//! Updated Dec 9, 2025: Added first-class unified string support with Str type and StringLit node.
//! Updated Dec 13, 2025: Added FString node for f-strings; BinaryOp for + concat sugar.
//! Updated Dec 16, 2025: Added TryProp for ? prop, DictLit for dict literals, Subscript for [] access, Return for explicit return; changed Assign to support complex lhs (e.g., subscripts); added single_line flag to FuncDef.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstNode {
    /// Full program as list of nodes.
    Program(Vec<AstNode>),
    /// Trait/concept definition with methods.
    ConceptDef { name: String, methods: Vec<AstNode> },
    /// Impl block for concept on type.
    ImplBlock {
        concept: String,
        ty: String,
        body: Vec<AstNode>,
    },
    /// Method signature in impl/concept.
    Method {
        name: String,
        params: Vec<(String, String)>,
        ret: String,
        generics: Vec<String>,
    },
    /// Function definition.
    FuncDef {
        name: String,
        generics: Vec<String>,
        params: Vec<(String, String)>,
        ret: String,
        body: Vec<AstNode>,
        attrs: Vec<String>,
        ret_expr: Option<Box<AstNode>>,
        single_line: bool, // New: flag for single-line syntax
    },
    /// Enum definition.
    EnumDef { name: String, variants: Vec<String> },
    /// Struct definition.
    StructDef {
        name: String,
        fields: Vec<(String, String)>,
    },
    /// Method/trait call, with structural flag for hybrid dispatch.
    Call {
        receiver: Option<Box<AstNode>>,
        method: String,
        args: Vec<AstNode>,
        type_args: Vec<String>,
        structural: bool,
    },
    /// Path call (A::B).
    PathCall {
        path: Vec<String>,
        method: String,
        args: Vec<AstNode>,
    },
    /// Actor spawn: spawn func(args).
    Spawn { func: String, args: Vec<AstNode> },
    /// Integer literal.
    Lit(i64),
    /// Unified UTF-8 owned string literal.
    StringLit(String),
    /// F-string: f"hello {expr}!".
    FString(Vec<AstNode>), // Alternating Lit(StringLit) and exprs
    /// Variable reference.
    Var(String),
    /// Assignment.
    Assign(Box<AstNode>, Box<AstNode>), // Updated: lhs can be Var or Subscript
    /// Binary operation (e.g., + for concat).
    BinaryOp {
        op: String, // "add", "concat", etc.
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    /// TimingOwned: Constant-time owned value abstraction.
    TimingOwned { ty: String, inner: Box<AstNode> },
    /// Defer statement for RAII cleanup.
    Defer(Box<AstNode>),
    /// Error propagation: expr?
    TryProp { expr: Box<AstNode> },
    /// Dict literal: {key: val, ...}
    DictLit { entries: Vec<(AstNode, AstNode)> },
    /// Subscript access: base[index]
    Subscript {
        base: Box<AstNode>,
        index: Box<AstNode>,
    },
    /// Explicit return statement.
    Return(Box<AstNode>),
}
