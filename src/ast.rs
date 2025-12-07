// src/ast.rs
//! Abstract Syntax Tree for Zeta.
//! Defines nodes for expressions, statements, definitions, and algebraic constructs.
//! Extended for self-host: enums, structs, strings, path calls.
//! Added: generics in FuncDef/Method, structural flag in Call for hybrid dispatch.

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
        generics: Vec<String>, // New: generics support
    },
    /// Function definition.
    FuncDef {
        name: String,
        generics: Vec<String>, // Updated: now parsed
        params: Vec<(String, String)>,
        ret: String,
        body: Vec<AstNode>,
        attrs: Vec<String>,
        ret_expr: Option<Box<AstNode>>,
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
        structural: bool, // New: true for structural dispatch (e.g., method?)
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
    /// String literal.
    StringLit(String),
    /// Variable reference.
    Var(String),
    /// Assignment.
    Assign(String, Box<AstNode>),
    /// TimingOwned: Constant-time owned value abstraction.
    TimingOwned { ty: String, inner: Box<AstNode> },
    /// Defer statement for RAII cleanup.
    Defer(Box<AstNode>),
}
