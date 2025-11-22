// src/parser.rs
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as nom_i64, multispace0, satisfy};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use nom::Parser;

fn ws<O>(inner: impl Parser<&str, O, nom::error::Error<&str>>) -> impl Parser<&str, O, nom::error::Error<&str>> {
    delimited(multispace0, inner, multispace0)
}

fn ident_parser() -> impl Parser<&str, String, nom::error::Error<&str>> {
    let first = satisfy(|c| c.is_alphabetic() || c == '_');
    let rest = many0(satisfy(|c| c.is_alphanumeric() || c == '_'));
    first.and_then(|c: char| rest.map(move |rest: Vec<char>| std::iter::once(c).chain(rest).collect()))
}

fn literal_parser() -> impl Parser<&str, AstNode, nom::error::Error<&str>> {
    map(nom_i64, AstNode::Lit)
}

fn variable_parser() -> impl Parser<&str, AstNode, nom::error::Error<&str>> {
    map(ident_parser(), AstNode::Var)
}

fn expr_parser() -> impl Parser<&str, AstNode, nom::error::Error<&str>> {
    let left = alt((literal_parser(), variable_parser()));
    let op = ws(tag("+"));
    let right = alt((literal_parser(), variable_parser()));
    left.and_then(move |left| op.and_then(move |_| right.map(move |right| AstNode::Call {
        receiver: Some(Box::new(left)),
        method: "add".to_string(),
        args: vec![right],
        type_args: vec![],
    })))
}

fn func_body_parser() -> impl Parser<&str, Vec<AstNode>, nom::error::Error<&str>> {
    delimited(ws(tag("{")), many0(ws(expr_parser())), ws(tag("}")))
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let fn_kw = ws(tag("fn"));
    let name = ident_parser();
    let lparen = ws(tag("("));
    let rparen = ws(tag(")"));
    let ret_type = opt(preceded(ws(tag("->")), ws(ident_parser())));
    let body = func_body_parser();

    tuple((
        fn_kw,
        name,
        lparen,
        rparen,
        ret_type,
        body,
    ))
    .map(|((_, name), _, _, _, ret, body)| AstNode::FuncDef {
        name,
        generics: vec![],
        params: vec![],
        ret: ret.map(|s| s.to_string()).unwrap_or_else(|| "i64".to_string()),
        body,
        attrs: vec![],
        ret_expr: None,
    })
    .parse(input)
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(ws(parse_func)).parse(input)
}
