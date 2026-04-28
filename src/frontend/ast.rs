// src/frontend/ast.rs
//! Defines the Abstract Syntax Tree (AST) nodes for the Zeta language.
//! Represents programs, definitions, expressions, statements, and algebraic constructs.

/// Generic parameter in function, struct, enum, concept, or impl definitions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericParam {
    /// Type parameter with optional trait bounds (e.g., T, T: Clone + Debug)
    Type {
        name: String,
        bounds: Vec<String>,
    },
    /// Lifetime parameter (e.g., 'a, 'static)
    Lifetime {
        name: String,
    },
    /// Const parameter with type (e.g., const N: usize, const SIZE: u32)
    Const {
        name: String,
        ty: String,
    },
}

/// Match expression arm with pattern and body.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchArm {
    /// Pattern to match against
    pub pattern: Box<AstNode>,
    /// Guard expression (optional)
    pub guard: Option<Box<AstNode>>,
    /// Body expression to evaluate if pattern matches
    pub body: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstNode {
    /// Full program as a sequence of top-level nodes.
    Program(Vec<AstNode>),
    /// Concept (trait) definition with method signatures.
    ConceptDef {
        name: String,
        generics: Vec<GenericParam>,
        lifetimes: Vec<String>, // Lifetime parameters like 'a, 'b
        methods: Vec<AstNode>,
        associated_types: Vec<AstNode>,
        attrs: Vec<String>,
        doc: String,
        /// Visibility: true for public, false for private (default)
        pub_: bool,
        /// Where clauses: e.g., where T: Clone, U: Debug + Display
        where_clauses: Vec<(String, Vec<String>)>,
        /// Supertraits: concepts this concept inherits from
        supertraits: Vec<String>,
    },
    /// Implementation block for a concept on a type.
    ImplBlock {
        concept: String,
        generics: Vec<GenericParam>,
        lifetimes: Vec<String>, // Lifetime parameters like 'a, 'b
        ty: String,
        body: Vec<AstNode>,
        attrs: Vec<String>,
        doc: String,
        /// Where clauses: e.g., where T: Clone, U: Debug + Display
        where_clauses: Vec<(String, Vec<String>)>,
    },
    /// Method signature within a concept or implementation.
    Method {
        name: String,
        params: Vec<(String, String)>,
        ret: String,
        generics: Vec<GenericParam>,
        lifetimes: Vec<String>, // Lifetime parameters like 'a, 'b
        attrs: Vec<String>,
        doc: String,
        /// Where clauses: e.g., where T: Clone, U: Debug + Display
        where_clauses: Vec<(String, Vec<String>)>,
        /// Optional body for default implementations in concepts
        body: Option<Vec<AstNode>>,
    },
    /// Function definition with parameters, return type, and body.
    FuncDef {
        name: String,
        generics: Vec<GenericParam>,
        lifetimes: Vec<String>, // Lifetime parameters like 'a, 'b
        params: Vec<(String, String)>,
        ret: String,
        body: Vec<AstNode>,
        attrs: Vec<String>,
        ret_expr: Option<Box<AstNode>>,
        single_line: bool,
        doc: String,
        /// Visibility: true for public, false for private (default)
        pub_: bool,
        /// Async flag: true for async functions
        async_: bool,
        /// Const flag: true for const functions (compile-time evaluable)
        const_: bool,
        /// Comptime flag: true for comptime functions (must be evaluated at compile time)
        comptime_: bool,
        /// Where clauses: e.g., where T: Clone, U: Debug + Display
        where_clauses: Vec<(String, Vec<String>)>,
    },
    ExternFunc {
        name: String,
        generics: Vec<GenericParam>,
        lifetimes: Vec<String>, // Lifetime parameters like 'a, 'b
        params: Vec<(String, String)>,
        ret: String,
        /// Where clauses: e.g., where T: Clone, U: Debug + Display
        where_clauses: Vec<(String, Vec<String>)>,
    },
    /// Enumeration definition with variants.
    EnumDef {
        name: String,
        generics: Vec<GenericParam>,
        lifetimes: Vec<String>, // Lifetime parameters like 'a, 'b
        variants: Vec<(String, Vec<String>)>,
        attrs: Vec<String>,
        doc: String,
        /// Visibility: true for public, false for private (default)
        pub_: bool,
        /// Where clauses: e.g., where T: Clone, U: Debug + Display
        where_clauses: Vec<(String, Vec<String>)>,
    },
    /// Structure definition with fields.
    StructDef {
        name: String,
        generics: Vec<GenericParam>,
        lifetimes: Vec<String>, // Lifetime parameters like 'a, 'b
        fields: Vec<(String, String)>,
        attrs: Vec<String>,
        doc: String,
        /// Visibility: true for public, false for private (default)
        pub_: bool,
        /// Where clauses: e.g., where T: Clone, U: Debug + Display
        where_clauses: Vec<(String, Vec<String>)>,
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
        type_args: Vec<String>,
    },
    /// Actor spawn expression with function and arguments.
    Spawn { func: String, args: Vec<AstNode> },
    /// Integer literal value.
    Lit(i64),
    /// Float literal value.
    FloatLit(String),
    /// String literal as owned UTF-8 string.
    StringLit(String),
    /// Formatted string with interpolated expressions.
    FString(Vec<AstNode>),
    /// Variable reference by name.
    Var(String),
    /// Assignment to a target (variable or subscript).
    Assign(Box<AstNode>, Box<AstNode>),
    /// Compound assignment (+=, -=, *=, /=, %=).
    AssignOp {
        op: String,
        target: Box<AstNode>,
        value: Box<AstNode>,
    },
    /// Binary operation between two expressions.
    BinaryOp {
        op: String,
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    /// Range expression (exclusive): start..end
    Range {
        start: Box<AstNode>,
        end: Box<AstNode>,
        inclusive: bool,
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
    /// Field access on a base expression.
    FieldAccess { base: Box<AstNode>, field: String },
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
    TypeAlias {
        name: String,
        ty: String,
        /// Visibility: true for public, false for private (default)
        pub_: bool,
    },
    /// Associated type declaration in a concept.
    AssociatedType {
        name: String,
        /// Optional default type
        default: Option<String>,
        /// Optional trait bounds
        bounds: Vec<String>,
    },
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
    /// While loop statement.
    While {
        cond: Box<AstNode>,
        body: Vec<AstNode>,
    },
    /// Unsafe block.
    Unsafe { body: Vec<AstNode> },
    /// Comptime block (compile-time evaluated block).
    ComptimeBlock { body: Vec<AstNode> },
    /// Macro call.
    MacroCall { name: String, args: Vec<AstNode> },
    /// Macro definition (macro_rules!).
    MacroDef { name: String, patterns: String },
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
    /// Array repeat literal: [value; size]
    ArrayRepeat {
        value: Box<AstNode>,
        size: Box<AstNode>,
    },
    /// Dynamic array literal: [dynamic]T{...}
    DynamicArrayLit {
        elem_type: String,
        elements: Vec<AstNode>,
    },
    /// Boolean literal.
    Bool(bool),
    /// Constant definition.
    ConstDef {
        name: String,
        ty: String,
        value: Box<AstNode>,
        /// Attributes (e.g., #[ai_opt], #[inline])
        attrs: Vec<String>,
        /// Visibility: true for public, false for private (default)
        pub_: bool,
        /// Compile-time flag: true for comptime variables
        comptime_: bool,
    },
    /// Match expression with scrutinee and arms.
    Match {
        scrutinee: Box<AstNode>,
        arms: Vec<MatchArm>,
    },
    /// Await expression for async functions.
    Await(Box<AstNode>),
    /// Module definition with nested items.
    ModDef {
        name: String,
        items: Vec<AstNode>,
        /// Visibility: true for public, false for private (default)
        pub_: bool,
        attrs: Vec<String>,
    },
    /// Range pattern (e.g., 1..=10, 'a'..='z')
    RangePattern {
        start: Box<AstNode>,
        end: Box<AstNode>,
        inclusive: bool,
    },
    /// Binding pattern with @ (e.g., x @ 1..=10)
    BindPattern {
        name: String,
        pattern: Box<AstNode>,
    },
    /// Or pattern for multiple alternatives (e.g., 1 | 2 | 3)
    OrPattern(Vec<AstNode>),
    /// Type-annotated pattern (e.g., x: i32, s: string[identity:read])
    TypeAnnotatedPattern {
        pattern: Box<AstNode>,
        ty: String,
    },
}
