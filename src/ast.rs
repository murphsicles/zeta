// src/ast.rs
#[derive(Debug, Clone)]
pub enum AstNode {
    ConceptDef { name: String, params: Vec<String>, methods: Vec<AstNode> },
    Method { name: String, params: Vec<(String, String)>, ret: String },
    ImplBlock { concept: String, ty: String, body: Vec<AstNode> },
    FuncDef {
        name: String,
        generics: Vec<String>,
        params: Vec<(String, String)>,
        ret: String,
        body: Vec<AstNode>,
        where_clause: Option<Vec<(String, String)>>,
        attrs: Vec<String>,
        ret_expr: Option<Box<AstNode>>,
    },
    ActorDef { name: String, state: Vec<(String, String)>, methods: Vec<AstNode> },
    AsyncFn { name: String, params: Vec<(String, String)>, ret: String, body: Vec<AstNode> },
    SpawnActor { actor_ty: String, init_args: Vec<String> },
    Await { expr: Box<AstNode> },
    Call {
        receiver: Box<AstNode>,
        method: String,
        args: Vec<AstNode>,
    },
    Lit(i64),
    Var(String),
    Assign(String, Box<AstNode>),
    Defer(Box<AstNode>),
    TimingOwned { ty: String, inner: Box<AstNode> },
    Derive { ty: String, traits: Vec<String> },
    StructDef { name: String, fields: Vec<(String, String)> },
}
