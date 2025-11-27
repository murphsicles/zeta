// src/ast.rs
//! Abstract Syntax Tree for Zeta.
//! Defines nodes for expressions, statements, definitions, and algebraic constructs.

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
    },
    /// Method/trait call.
    Call {
        receiver: Option<Box<AstNode>>,
        method: String,
        args: Vec<AstNode>,
        type_args: Vec<String>,
    },
    /// Integer literal.
    Lit(i64),
    /// Variable reference.
    Var(String),
    /// Assignment.
    Assign(String, Box<AstNode>),
    /// TimingOwned: Constant-time owned value abstraction.
    TimingOwned { ty: String, inner: Box<AstNode> },
    /// Defer statement for RAII cleanup.
    Defer(Box<AstNode>),
}
