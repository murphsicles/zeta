// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, and defer.

use crate::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, i64 as nom_i64, multispace0};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, preceded, tuple};

/// Whitespace wrapper for parsers.
fn ws<'a, O>(
    inner: impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

/// Parses an identifier (alpha + alphanum).
fn ident<'a>() -> impl Parser<&'a str, Output = String, Error = nom::error::Error<&'a str>> {
    map(alpha1.and(alphanumeric0), |(first, rest): (&str, &str)| {
        first.to_string() + rest
    })
}

/// Parses an integer literal.
fn literal<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(nom_i64, AstNode::Lit)
}

/// Parses a variable reference.
fn variable<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(ident(), AstNode::Var)
}

/// Parses TimingOwned<ty> (expr).
fn timing_owned<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        tuple((
            ws(tag("TimingOwned")),
            ws(delimited(tag("<"), ident(), tag(">"))),
            ws(tag("(")),
            ws(expr()),
            ws(tag(")")),
        )),
        |((_, ty), _, _, inner, _)| AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        },
    )
}

/// Parses expressions: lit/var + optional + lit/var (as add call) | TimingOwned.
fn expr<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let left = alt((literal(), variable(), timing_owned()));
    let add = ws(tag("+")).and(alt((literal(), variable(), timing_owned())));
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

/// Parses a defer statement: defer expr.
fn defer_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(preceded(ws(tag("defer")), ws(expr())), |e| AstNode::Defer(Box::new(e)))
}

/// Parses a statement: defer or expr.
fn stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((defer_stmt(), expr()))
}

/// Parses function body: { stmt* }.
fn func_body<'a>() -> impl Parser<&'a str, Output = Vec<AstNode>, Error = nom::error::Error<&'a str>>
{
    delimited(ws(tag("{")), many0(ws(stmt())), ws(tag("}")))
}

/// Parses a full function definition: fn name() -> ret { body }.
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
            let ret: String = ret_opt
                .map(|s: String| s)
                .unwrap_or_else(|| "i64".to_string());
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

/// Entry point: Parses multiple top-level functions.
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(ws(parse_func())), multispace0).parse(input)
}
