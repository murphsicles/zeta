// src/ast.rs
#[derive(Debug, Clone)]
pub enum AstNode {
    ConceptDef {
        name: String,
        params: Vec<String>,
        methods: Vec<AstNode>,
    },
    Method {
        name: String,
        params: Vec<(String, String)>,
        ret: String,
    },
    ImplBlock {
        concept: String,
        ty: String,
        body: Vec<AstNode>,
    },
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
    Call {
        receiver: String,
        method: String,
        args: Vec<String>,
    },
    Lit(i64),
    Var(String),
    Borrow(String),
    Assign(String, Box<AstNode>),
    Let {
        name: String,
        rhs: Box<AstNode>,
    },
    Defer(Box<AstNode>),
    ActorDef {
        name: String,
        methods: Vec<AstNode>,
    },
    SpawnActor {
        actor_ty: String,
        init_args: Vec<String>,
    },
    TimingOwned {
        ty: String,
        inner: Box<AstNode>,
    },
    Derive {
        ty: String,
        traits: Vec<String>,
    },
    StructDef {
        name: String,
        fields: Vec<(String, String)>,
    },
}

impl AstNode {
    pub fn receiver_str(&self) -> Option<&str> {
        match self {
            AstNode::Call { receiver, .. } => Some(receiver),
            _ => None,
        }
    }

    pub fn arg_str(&self) -> Option<&str> {
        match self {
            AstNode::Var(s) | AstNode::Borrow(s) => Some(s),
            _ => None,
        }
    }
}
