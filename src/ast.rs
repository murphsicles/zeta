// src/ast.rs
#[derive(Debug, Clone)]
pub enum AstNode {
    ConceptDef { name: String, params: Vec<String>, methods: Vec<AstNode> },
    Method { name: String, params: Vec<(String, String)>, ret: String },
    ImplBlock { concept: String, ty: String, body: Vec<AstNode> },
    FuncDef { name: String, generics: Vec<String>, params: Vec<(String, String)>, ret: String, body: Vec<AstNode>, where_clause: Option<Vec<(String, String)>> },
    Call { method: String, receiver: String, args: Vec<String> },
    Lit(i64),
    Var(String),
    Borrow(String),
    Assign(String, Box<AstNode>),
    Defer(Box<AstNode>), // defer { expr }
}
