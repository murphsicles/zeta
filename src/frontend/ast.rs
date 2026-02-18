// src/frontend/ast.rs
//! Defines the Abstract Syntax Tree (AST) nodes for the Zeta language.
//! Represents programs, definitions, expressions, statements, and algebraic constructs.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstNode {
    /// Full program as a sequence of top-level nodes.
    Program(Vec<AstNode>),
    /// Concept (trait) definition with method signatures.
    ConceptDef {
        name: String,
        generics: Vec<String>,
        methods: Vec<AstNode>,
        doc: String,
    },
    /// Implementation block for a concept on a type.
    ImplBlock {
        concept: String,
        generics: Vec<String>,
        ty: String,
        body: Vec<AstNode>,
        doc: String,
    },
    /// Method signature within a concept or implementation.
    Method {
        name: String,
        params: Vec<(String, String)>,
        ret: String,
        generics: Vec<String>,
        doc: String,
    },
    /// Function definition with parameters, return type, and body.
    FuncDef {
        name: String,
        generics: Vec<String>,
        params: Vec<(String, String)>,
        ret: String,
        body: Vec<AstNode>,
        attrs: Vec<String>,
        ret_expr: Option<Box<AstNode>>,
        single_line: bool,
        doc: String,
    },
    ExternFunc {
        name: String,
        params: Vec<(String, String)>,
        ret: String,
    },
    /// Enumeration definition with variants.
    EnumDef {
        name: String,
        variants: Vec<(String, Vec<String>)>,
        doc: String,
    },
    /// Structure definition with fields.
    StructDef {
        name: String,
        fields: Vec<(String, String)>,
        doc: String,
    },
    /// Method or function call, with optional receiver and structural dispatch flag.
    Call {
        receiver: Option<Box<AstNode>>,
        method: String,
        args: Vec<AstNode>,
        type_args: Vec<String>,
        structural: bool,
    },
    /// Path-based call for static or associated functions.
    PathCall {
        path: Vec<String>,
        method: String,
        args: Vec<AstNode>,
    },
    /// Actor spawn expression with function and arguments.
    Spawn { func: String, args: Vec<AstNode> },
    /// Integer literal value.
    Lit(i64),
    /// String literal as owned UTF-8 string.
    StringLit(String),
    /// Formatted string with interpolated expressions.
    FString(Vec<AstNode>),
    /// Variable reference by name.
    Var(String),
    /// Assignment to a target (variable or subscript).
    Assign(Box<AstNode>, Box<AstNode>),
    /// Binary operation between two expressions.
    BinaryOp {
        op: String,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    /// Timing-owned value abstraction for constant-time operations.
    TimingOwned { ty: String, inner: Box<AstNode> },
    /// Defer statement for cleanup actions.
    Defer(Box<AstNode>),
    /// Error propagation operator on an expression.
    TryProp { expr: Box<AstNode> },
    /// Dictionary literal with key-value pairs.
    DictLit { entries: Vec<(AstNode, AstNode)> },
    /// Subscript access on a base expression.
    Subscript {
        base: Box<AstNode>,
        index: Box<AstNode>,
    },
    /// Explicit return statement with value.
    Return(Box<AstNode>),
    /// Conditional statement.
    If {
        cond: Box<AstNode>,
        then: Vec<AstNode>,
        else_: Vec<AstNode>,
    },
    /// Expression used as a statement (for side-effects, e.g. function call).
    ExprStmt { expr: Box<AstNode> },
    /// Use statement for importing paths.
    Use { path: Vec<String> },
    /// Type alias definition.
    TypeAlias { name: String, ty: String },
    /// Unary operation on an expression.
    UnaryOp { op: String, expr: Box<AstNode> },
    /// Type cast expression.
    Cast { expr: Box<AstNode>, ty: String },
    /// Closure expression with parameters and body.
    Closure {
        params: Vec<String>,
        body: Box<AstNode>,
    },
    /// Block expression with statements.
    Block { body: Vec<AstNode> },
    /// If-let conditional with pattern matching.
    IfLet {
        pattern: Box<AstNode>,
        expr: Box<AstNode>,
        then: Vec<AstNode>,
        else_: Vec<AstNode>,
    },
    /// Ignore pattern (_).
    Ignore,
    /// Tuple pattern or expression.
    Tuple(Vec<AstNode>),
    /// Struct pattern for matching.
    StructPattern {
        variant: String,
        fields: Vec<(String, AstNode)>,
        rest: bool,
    },
    /// Let binding statement.
    Let {
        mut_: bool,
        pattern: Box<AstNode>,
        ty: Option<String>,
        expr: Box<AstNode>,
    },
    /// For loop statement.
    For {
        pattern: Box<AstNode>,
        expr: Box<AstNode>,
        body: Vec<AstNode>,
    },
    /// Loop statement.
    Loop { body: Vec<AstNode> },
    /// Unsafe block.
    Unsafe { body: Vec<AstNode> },
    /// Macro call.
    MacroCall { name: String, args: Vec<AstNode> },
    /// Struct literal.
    StructLit {
        variant: String,
        fields: Vec<(String, AstNode)>,
    },
    /// Break statement.
    Break(Option<Box<AstNode>>),
    /// Continue statement.
    Continue(Option<Box<AstNode>>),
    /// Array literal.
    ArrayLit(Vec<AstNode>),
    /// Boolean literal.
    Bool(bool),
}
