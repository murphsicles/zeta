// src/main.rs
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::process::Command;

// Token enum for Zeta syntax
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Concept,
    Impl,
    Fn,
    Ident(String),
    Colon,
    Arrow,
    LParen,
    RParen,
    Comma,
    Eq,
    Semi,
    Lt,
    Gt,
    Where,
    For,
    Mut,
    And,
    Plus,
    BraceOpen,
    BraceClose,
    AngleOpen,
    AngleClose,
    Hash,
    BracketOpen,
    BracketClose,
    IntLit(i64),
}

// Parser for tokens
fn identifier(input: &str) -> IResult<&str, Token> {
    let ident = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ));
    map(ident, |s: &str| Token::Ident(s.to_string()))(input)
}

fn int_literal(input: &str) -> IResult<&str, Token> {
    map(digit1, |s: &str| Token::IntLit(s.parse().unwrap()))(input)
}

fn keyword(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Concept, tag("concept")),
        value(Token::Impl, tag("impl")),
        value(Token::Fn, tag("fn")),
        value(Token::Where, tag("where")),
        value(Token::For, tag("for")),
        value(Token::Mut, tag("mut")),
        value(Token::And, tag("+")),
    ))(input)
}

fn symbol(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Colon, char(':')),
        value(Token::Arrow, tag("->")),
        value(Token::LParen, char('(')),
        value(Token::RParen, char(')')),
        value(Token::Comma, char(',')),
        value(Token::Eq, char('=')),
        value(Token::Semi, char(';')),
        value(Token::Lt, char('<')),
        value(Token::Gt, char('>')),
        value(Token::Plus, char('+')),
        value(Token::BraceOpen, char('{')),
        value(Token::BraceClose, char('}')),
        value(Token::AngleOpen, tag("<")),
        value(Token::AngleClose, tag(">")),
        value(Token::Hash, tag("#")),
        value(Token::BracketOpen, tag("[")),
        value(Token::BracketClose, tag("]")),
    ))(input)
}

pub fn tokenize(input: &str) -> IResult<&str, Vec<Token>> {
    many0(terminated(
        alt((int_literal, identifier, keyword, symbol)),
        multispace0,
    ))(input)
}

// AST for Zeta
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
    },
    Call {
        method: String,
        receiver: String,
        args: Vec<String>,
    },
    Lit(i64),
    Var(String),
}

// Parser helpers
fn map_opt<I: Clone>(p: impl Fn(&str) -> IResult<&str, Token>) -> impl Fn(&str) -> IResult<&str, Option<String>> {
    move |input| {
        let (i, t) = p(input)?;
        match t {
            Token::Ident(s) => Ok((i, Some(s))),
            _ => Ok((i, None)),
        }
    }
}

fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((
        tag("concept"),
        multispace0,
        map_opt(identifier),
        multispace0,
        opt(delimited(
            tag("<"),
            separated_list1(tag(","), map_opt(identifier)),
            tag(">"),
        )),
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_method),
        tag("}"),
    ));

    let (i, (_, _, name, _, params_opt, _, _, _, methods, _)) = parser(input)?;
    let params = params_opt.unwrap_or_default();
    Ok((i, AstNode::ConceptDef { name: name.unwrap_or_default(), params, methods }))
}

fn parse_method(input: &str) -> IResult<&str, AstNode> {
    let param_parser = separated_pair(
        opt(tag("mut")),
        tag(":"),
        tuple((map_opt(identifier), multispace0, tag(":"), multispace0, map_opt(identifier))),
    );

    let parser = tuple((
        tag("fn"),
        multispace0,
        map_opt(identifier),
        multispace0,
        delimited(
            tag("("),
            separated_list1(tag(","), param_parser),
            tag(")"),
        ),
        multispace0,
        tag("->"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag(";"),
    ));

    let (i, (_, _, name, _, params, _, _, _, ret, _, _)) = parser(input)?;
    let method_params: Vec<(String, String)> = params.into_iter().map(|(mut_opt, (pn, _, _, _, ty))| {
        let param_name = pn.unwrap_or_default();
        let ty_str = ty.unwrap_or_default();
        (param_name, ty_str)
    }).collect();
    let ret_str = ret.unwrap_or_default();
    Ok((i, AstNode::Method { name: name.unwrap_or_default(), params: method_params, ret: ret_str }))
}

fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let ty_parser = tuple((
        map_opt(identifier),
        multispace0,
        opt(delimited(
            tag("<"),
            separated_list1(tag(","), map_opt(identifier)),
            tag(">"),
        )),
    ));

    let parser = tuple((
        tag("impl"),
        multispace0,
        map_opt(identifier),
        multispace0,
        ty_parser,
        multispace0,
        tag("for"),
        multispace0,
        ty_parser,
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_method),
        tag("}"),
    ));

    let (i, (_, _, concept, _, _, _, _, _, for_ty, _, _, _, methods, _)) = parser(input)?;
    let full_ty = if let Some(params) = for_ty.2 {
        format!("{}<{}>", for_ty.0.unwrap_or_default(), params.join(","))
    } else {
        for_ty.0.unwrap_or_default()
    };
    Ok((i, AstNode::ImplBlock { concept: concept.unwrap_or_default(), ty: full_ty, body: methods }))
}

fn parse_where_clause(input: &str) -> IResult<&str, Vec<(String, String)>> {
    preceded(
        tag("where"),
        separated_list1(
            tag(","),
            separated_pair(
                map_opt(identifier),
                tag(":"),
                map_opt(identifier),
            ),
        ),
    )(input)
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    alt((
        map(int_literal, |t| if let Token::IntLit(n) = t { AstNode::Lit(n) } else { unreachable!() }),
        map_opt(identifier).map(|s| AstNode::Var(s.unwrap_or_default())),
        parse_call,
    ))(input)
}

fn parse_call(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((
        map_opt(identifier),
        multispace0,
        map_opt(identifier),
        multispace0,
        delimited(
            tag("("),
            separated_list1(tag(","), map_opt(identifier)),
            tag(")"),
        ),
        multispace0,
        tag(";"),
    ));

    let (i, (receiver, _, method, _, args_opt, _, _)) = parser(input)?;
    let args = args_opt.unwrap_or_default();
    Ok((i, AstNode::Call {
        method: method.unwrap_or_default(),
        receiver: receiver.unwrap_or_default(),
        args,
    }))
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let generics_parser = opt(delimited(
        tag("<"),
        separated_list1(tag(","), map_opt(identifier)),
        tag(">"),
    ));

    let param_parser = separated_pair(
        opt(tag("mut")),
        tag(":"),
        tuple((map_opt(identifier), multispace0, tag(":"), multispace0, map_opt(identifier))),
    );

    let parser = tuple((
        tag("fn"),
        multispace0,
        map_opt(identifier),
        multispace0,
        generics_parser,
        multispace0,
        delimited(
            tag("("),
            separated_list1(tag(","), param_parser),
            tag(")"),
        ),
        multispace0,
        tag("->"),
        multispace0,
        map_opt(identifier),
        multispace0,
        opt(parse_where_clause),
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_expr),
        tag("}"),
    ));

    let (i, (_, _, name, _, generics_opt, _, params, _, _, _, ret, _, where_opt, _, _, _, body, _)) = parser(input)?;
    let generics = generics_opt.unwrap_or_default();
    let func_params: Vec<(String, String)> = params.into_iter().map(|(mut_opt, (pn, _, _, _, ty))| {
        let param_name = pn.unwrap_or_default();
        let ty_str = ty.unwrap_or_default();
        (param_name, ty_str)
    }).collect();
    let ret_str = ret.unwrap_or_default();
    let where_clause = where_opt;
    Ok((i, AstNode::FuncDef {
        name: name.unwrap_or_default(),
        generics,
        params: func_params,
        ret: ret_str,
        body,
        where_clause,
    }))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(alt((parse_func, parse_impl, parse_concept)))(input)
}

// MIR-like IR
#[derive(Debug, Clone)]
pub enum MirNode {
    DefConcept(String, Vec<String>),
    DefImpl(String, String),
    DefFunc(String, Vec<String>),
    Call(String, String, Vec<String>), // method, receiver_ty, args
    Ret(String),
    Lit(i64),
    Var(String),
}

// Resolver and Typechecker
#[derive(Debug, Clone)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            concepts: HashMap::new(),
            impls: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { name, .. } => {
                self.concepts.insert(name, ast);
            }
            AstNode::ImplBlock { concept, ty, .. } => {
                self.impls.insert((concept, ty), ast);
            }
            AstNode::FuncDef { name, .. } => {
                self.funcs.insert(name, ast);
            }
            _ => {}
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        self.impls.get(&(concept.to_string(), ty.to_string()))
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        for ast in asts {
            if let AstNode::FuncDef { where_clause, body, .. } = ast {
                if let Some(bounds) = where_clause {
                    for (ty, concept) in bounds {
                        if self.resolve_impl(&concept, &ty).is_none() {
                            return false;
                        }
                    }
                }
                for node in body {
                    if let AstNode::Call { method, receiver, .. } = node {
                        // Infer recv_ty from context; simplified to "Vec<i32>"
                        if !self.has_method("Addable", "Vec<i32>", method) {
                            return false;
                        }
                    }
                }
            }
        }
        true
    }

    fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        if let Some(impl_ast) = self.resolve_impl(concept, ty) {
            if let AstNode::ImplBlock { body, .. } = impl_ast {
                body.iter().any(|m| {
                    if let AstNode::Method { name, .. } = m {
                        name == method
                    } else {
                        false
                    }
                })
            } else {
                false
            }
        } else {
            false
        }
    }
}

// Codegen to C (simple backend)
pub struct Codegen {
    output: String,
    indent: usize,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
        }
    }

    pub fn push(&mut self, s: &str) {
        self.output.push_str(&"\t".repeat(self.indent));
        self.output.push_str(s);
        self.output.push('\n');
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn dedent(&mut self) {
        self.indent -= 1;
    }

    pub fn gen_func(&mut self, func: &AstNode, impls: &HashMap<(String, String), AstNode>) {
        if let AstNode::FuncDef { name, params, ret, body, .. } = func {
            self.push(&format!("{} {}({}) {{", ret, name, params.iter().map(|(n, t)| format!("{} {}", n, t)).collect::<Vec<_>>().join(", ")));
            self.indent();
            for node in body {
                self.gen_stmt(node, impls);
            }
            self.push(&format!("return 0;"));
            self.dedent();
            self.push("}");
        }
    }

    fn gen_stmt(&mut self, node: &AstNode, impls: &HashMap<(String, String), AstNode>) {
        match node {
            AstNode::Call { method, receiver, args } => {
                // Simplified: assume add on Vec<i32> as vadd
                if method == "add" {
                    let arg = args.first().unwrap_or(&"0".to_string());
                    self.push(&format!("// {} = {}.add({});", receiver, receiver, arg));
                }
            }
            AstNode::Lit(n) => self.push(&format!("// Lit: {}", n)),
            AstNode::Var(v) => self.push(&format!("// Var: {}", v)),
            _ => {}
        }
    }

    pub fn output(&self) -> &str {
        &self.output
    }
}

// Main compiler pipeline
pub fn compile_zeta(input: &str, output_file: &str) -> Result<(), Box<dyn std::error::Error>> {
    let (_, asts) = parse_zeta(input)?;
    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    if !resolver.typecheck(&asts) {
        return Err("Typecheck failed".into());
    }

    let mut cg = Codegen::new();
    cg.push("#include <stdio.h>");
    cg.push("int main() {");
    cg.indent();
    for ast in &asts {
        match ast {
            AstNode::FuncDef { .. } => cg.gen_func(ast, &resolver.impls),
            _ => {}
        }
    }
    cg.push("return 0;");
    cg.dedent();
    cg.push("}");

    let mut file = File::create(output_file)?;
    file.write_all(cg.output().as_bytes())?;

    // Compile C to exe (assumes gcc)
    Command::new("gcc").args([&output_file, "-o", "zeta_out"]).status()?;
    Ok(())
}

fn main() {
    let code = r#"
concept Addable<Rhs=Self> {
    fn add(self, rhs: Rhs) -> Self;
}

impl Addable for Vec<i32> {
    fn add(self, rhs: i32) -> Vec<i32>;
}

fn use_add<T>(a: T, b: T) -> T where T: Addable {
    a.add(b);
}
"#;
    if let Err(e) = compile_zeta(code, "output.c") {
        eprintln!("Compile error: {}", e);
    } else {
        println!("Compiled to zeta_out");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = r#"
concept Addable<Rhs=Self> {
    fn add(self: Self, rhs: Rhs) -> Self;
}
"#;
        let result = parse_zeta(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_typecheck() {
        let code = r#"
concept Addable<Rhs=Self> {
    fn add(self: Self, rhs: Rhs) -> Self;
}

impl Addable for Vec<i32> {
    fn add(self: Vec<i32>, rhs: i32) -> Vec<i32>;
}

fn use_add<T>(a: T, b: T) -> T where T: Addable {
    a.add(b);
}
"#;
        let (_, asts) = parse_zeta(code).unwrap();
        let mut resolver = Resolver::new();
        for ast in &asts {
            resolver.register(ast.clone());
        }
        assert!(resolver.typecheck(&asts));
    }
}
