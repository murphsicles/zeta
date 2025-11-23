// src/parser.rs
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as nom_i64, multispace0};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;
use nom::Parser;

fn ws<'a, O>(inner: impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

fn ident<'a>() -> impl Parser<&'a str, Output = String, Error = nom::error::Error<&'a str>> {
    map(
        nom::character::complete::alpha1.and(nom::character::complete::alphanum0),
        |(first, rest): (&str, &str)| format!("{}{}", first, rest),
    )
}

fn literal<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(nom_i64, AstNode::Lit)
}

fn variable<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(ident(), AstNode::Var)
}

fn expr<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let left = alt((literal(), variable()));
    let add = ws(tag("+")).and(alt((literal(), variable())));
    left.and(opt(add)).map(|(left, opt_add)| {
        if let Some((_, right)) = opt_add {
            AstNode::Call {
                receiver: Some(Box::new(left)),
                method: "add".to_string(),
                args: vec![right],
                type_args: vec![],
            }
        } else {
            left
        }
    })
}

fn func_body<'a>() -> impl Parser<&'a str, Output = Vec<AstNode>, Error = nom::error::Error<&'a str>> {
    delimited(ws(tag("{")), many0(ws(expr())), ws(tag("}")))
}

fn parse_func<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let fn_kw = ws(tag("fn"));
    let name = ident();
    let lparen = ws(tag("("));
    let rparen = ws(tag(")"));
    let ret_type = opt(preceded(ws(tag("->")), ws(ident())));
    let body = func_body();
    fn_kw
        .and(name)
        .and(lparen)
        .and(rparen)
        .and(ret_type)
        .and(body)
        .map(|(((((_, name), _), _), ret_opt), body)| {
            let ret: String = ret_opt.map(|s: &str| s.to_string()).unwrap_or_else(|| "i64".to_string());
            AstNode::FuncDef {
                name,
                generics: vec![],
                params: vec![],
                ret,
                body,
                attrs: vec![],
                ret_expr: None,
            }
        })
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(ws(parse_func())), multispace0).parse(input)
}
