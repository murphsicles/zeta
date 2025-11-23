// src/parser.rs
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as nom_i64, multispace0, satisfy};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;
use nom::Parser;

fn ws<'a, O>(inner: impl Parser<&'a str, O, nom::error::Error<&'a str>> + 'a) -> impl Parser<&'a str, O, nom::error::Error<&'a str>> + 'a {
    delimited(multispace0, inner, multispace0)
}

fn ident<'a>() -> impl Parser<&'a str, String, nom::error::Error<&'a str>> + 'a {
    let first = satisfy(|c| c.is_alphabetic() || c == '_');
    let rest = many0(satisfy(|c| c.is_alphanumeric() || c == '_'));
    map((first, rest), |(c, rest): (char, Vec<char>)| std::iter::once(c).chain(rest).collect())
}

fn literal<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> + 'a {
    map(nom_i64, AstNode::Lit)
}

fn variable<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> + 'a {
    map(ident::<'a>(), AstNode::Var)
}

fn expr<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> + 'a {
    let left = alt((literal::<'a>(), variable::<'a>()));
    let add = ws::<'a, _>(tag("+")).and(alt((literal::<'a>(), variable::<'a>())));
    map((left, opt(add)), |(left, opt_add)| {
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

fn func_body<'a>() -> impl Parser<&'a str, Vec<AstNode>, nom::error::Error<&'a str>> + 'a {
    delimited(
        ws::<'a, _>(tag("{")),
        many0(ws::<'a, _>(expr::<'a>())),
        ws::<'a, _>(tag("}")),
    )
}

fn parse_func<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> + 'a {
    let fn_kw = ws::<'a, _>(tag("fn"));
    let name = ident::<'a>();
    let lparen = ws::<'a, _>(tag("("));
    let rparen = ws::<'a, _>(tag(")"));
    let ret_type = opt(preceded(ws::<'a, _>(tag("->")), ws::<'a, _>(ident::<'a>())));
    let body = func_body::<'a>();
    map((fn_kw, name, lparen, rparen, ret_type, body), |(_, name, _, _, ret_opt, body)| AstNode::FuncDef {
        name,
        generics: vec![],
        params: vec![],
        ret: ret_opt.map(|s| s.to_string()).unwrap_or_else(|| "i64".to_string()),
        body,
        attrs: vec![],
        ret_expr: None,
    })
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(ws::<'_, _>(parse_func::<'_>())), multispace0).parse(input)
}
