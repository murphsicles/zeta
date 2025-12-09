// src/ast.rs
//! Abstract Syntax Tree for Zeta.
//! Defines nodes for expressions, statements, definitions, and algebraic constructs.
//! Extended for self-host: enums, structs, strings, path calls.
//! Added: generics in FuncDef/Method, structural flag in Call for hybrid dispatch.
//! Added: Pattern enum for match, Match node, FieldAccess node.
//! Added: generics in ImplBlock.
//! Added: DocComment for /// docs attached to defs.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    /// Literal pattern.
    Lit(i64),
    /// Variable binding pattern.
    Var(String),
    /// Enum variant pattern with subpatterns.
    Variant(String, Vec<Pattern>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstNode {
    /// Full program as list of nodes.
    Program(Vec<AstNode>),
    /// Trait/concept definition with methods.
    ConceptDef { name: String, methods: Vec<AstNode>, docs: Option<String> },
    /// Impl block for concept on type, now with generics.
    ImplBlock {
        generics: Vec<String>,
        concept: String,
        ty: String,
        body: Vec<AstNode>,
        docs: Option<String>,
    },
    /// Method signature in impl/concept.
    Method {
        name: String,
        params: Vec<(String, String)>,
        ret: String,
        generics: Vec<String>, // New: generics support
        docs: Option<String>,
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
        docs: Option<String>,
    },
    /// Enum definition.
    EnumDef { name: String, variants: Vec<String>, docs: Option<String> },
    /// Struct definition.
    StructDef {
        name: String,
        fields: Vec<(String, String)>,
        docs: Option<String>,
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
    /// Field access: expr.field.
    FieldAccess {
        receiver: Box<AstNode>,
        field: String,
    },
    /// TimingOwned: Constant-time owned value abstraction.
    TimingOwned { ty: String, inner: Box<AstNode> },
    /// Match expression: match expr { pat => expr, ... }.
    Match {
        expr: Box<AstNode>,
        arms: Vec<(Pattern, Box<AstNode>)>,
    },
    /// Defer statement for RAII cleanup.
    Defer(Box<AstNode>),
    /// Doc comment: /// text.
    DocComment(String),
}
