#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        receiver: Box<AstNode>,
        method: String,
        args: Vec<AstNode>,
    },
    Lit(i64),
    Var(String),
    Let {
        name: String,
        ty: Option<String>,
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
    ExprStmt(Box<AstNode>),
    Construct {
        ty: String,
        fields: Vec<(String, AstNode)>,
    },
}
