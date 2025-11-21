// src/ast.rs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstNode {
    Program(Vec<AstNode>),
    ConceptDef { name: String, methods: Vec<AstNode> },
    ImplBlock { concept: String, ty: String, body: Vec<AstNode> },
    Method { name: String, params: Vec<(String, String)>, ret: String },
    FuncDef {
        name: String,
        generics: Vec<String>,
        params: Vec<(String, String)>,
        ret: String,
        body: Vec<AstNode>,
        attrs: Vec<String>,
        ret_expr: Option<Box<AstNode>>,
    },
    Call {
        receiver: Option<Box<AstNode>>,
        method: String,
        args: Vec<AstNode>,
        type_args: Vec<String>,
    },
    Lit(i64),
    Var(String),
    Assign(String, Box<AstNode>),
    TimingOwned { ty: String, inner: Box<AstNode> },
}
