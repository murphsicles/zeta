// src/ast.rs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstNode {
    Program(Vec<AstNode>),
    ConceptDef { name: String, methods: Vec<AstNode> },
    StructDef { name: String, fields: Vec<(String, String)> },
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
    Add(Box<AstNode>, Box<AstNode>),
}
