// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, i64 as nom_i64, multispace0};
use nom::combinator::{map, opt, recursive};
use nom::multi::many0;
use nom::sequence::{delimited, preceded, tuple};
use nom::{IResult, Parser};

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

/// Parses method signature: name(params) -> ret.
fn method_sig<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let name = ident();
    let param_pair = ws(ident()).and(ws(tag(":"))).and(ws(ident()));
    let params = delimited(tag("("), many0(param_pair), tag(")"));
    let ret = opt(preceded(ws(tag("->")), ws(ident())));
    map(
        (name, params, ret),
        |(name, params, ret_opt): (String, Vec<((String, &str), String)>, Option<String>)| {
            let ret: String = ret_opt.unwrap_or_else(|| "i64".to_string());
            AstNode::Method {
                name,
                params: params.into_iter().map(|((n, _), t)| (n, t)).collect(),
                ret,
            }
        },
    )
}

/// Parses expressions: lit/var + optional + lit/var (as add call) | TimingOwned | call.
fn expr<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let rec_expr = recursive(|expr| {
        let timing_owned = map(
            (
                ws(tag("TimingOwned")),
                ws(delimited(tag("<"), ident(), tag(">"))),
                ws(tag("(")),
                ws(expr),
                ws(tag(")")),
            ),
            |(_, ty, _, inner, _)| AstNode::TimingOwned {
                ty,
                inner: Box::new(inner),
            },
        );
        let base = alt((literal(), variable(), timing_owned));
        let add = ws(tag("+")).and(base);
        let call = delimited(
            tag("("),
            many0(ws(expr())),
            tag(")"),
        ).and(base).map(|(args, recv)| AstNode::Call {
            receiver: Some(Box::new(recv)),
            method: "call".to_string(), // Placeholder; actual method from context
            args,
            type_args: vec![],
        });
        alt((call, base.and(opt(add)).map(|(left, opt_add)| {
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
        })))
    });
    rec_expr
}

/// Parses assignment: ident = expr;.
fn assign_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        tuple((ws(ident()), ws(tag("=")), ws(expr()), ws(tag(";")))),
        |(name, _, expr, _)| AstNode::Assign(name, Box::new(expr)),
    )
}

/// Parses a defer statement: defer expr;.
fn defer_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(tuple((ws(tag("defer")), ws(expr()), ws(tag(";")))), |(_, e, _)| {
        AstNode::Defer(Box::new(e))
    })
}

/// Parses spawn: spawn ident(args);.
fn spawn_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let kw = ws(tag("spawn"));
    let func = ident();
    let args = delimited(tag("("), many0(ws(expr())), tag(")"));
    map(tuple((kw, func, args, ws(tag(";")))), |(_, func, args, _)| AstNode::Spawn {
        func,
        args,
    })
}

/// Parses a statement: assign | spawn | defer | expr;.
fn stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((assign_stmt(), spawn_stmt(), defer_stmt(), tuple((ws(expr()), ws(tag(";")))).map(|(e, _)| e)))
}

/// Parses function body: { stmt* }.
fn func_body<'a>() -> impl Parser<&'a str, Output = Vec<AstNode>, Error = nom::error::Error<&'a str>>
{
    delimited(ws(tag("{")), many0(ws(stmt())), ws(tag("}")))
}

/// Parses a full function definition: fn name(param: i64) -> ret { body }.
fn parse_func<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let fn_kw = ws(tag("fn"));
    let name = ident();
    let param_pair = ws(ident()).and(ws(tag(":"))).and(ws(ident()));
    let params = delimited(tag("("), many0(param_pair), tag(")"));
    let ret_type = opt(preceded(ws(tag("->")), ws(ident())));
    let body = func_body();
    map(
        (fn_kw, name, params, ret_type, body),
        |(_, name, params, ret_opt, body)| {
            let ret: String = ret_opt.unwrap_or_else(|| "i64".to_string());
            AstNode::FuncDef {
                name,
                generics: vec![],
                params: params.into_iter().map(|((n, _), t)| (n, t)).collect(),
                ret,
                body,
                attrs: vec![],
                ret_expr: None,
            }
        },
    )
}

/// Parses concept definition: concept Name { methods }.
fn parse_concept<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    let kw = ws(tag("concept"));
    let name = ident();
    let body = delimited(ws(tag("{")), many0(ws(method_sig())), ws(tag("}")));
    map((kw, name, body), |(_, name, body)| AstNode::ConceptDef {
        name,
        methods: body,
    })
}

/// Parses impl block: impl Concept for Ty { methods }.
fn parse_impl<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let kw = ws(tag("impl"));
    let concept = ident();
    let for_kw = ws(tag("for"));
    let ty = ident();
    let body = delimited(ws(tag("{")), many0(ws(method_sig())), ws(tag("}")));
    map(
        (kw, concept, for_kw, ty, body),
        |(_, concept, _, ty, body)| AstNode::ImplBlock { concept, ty, body },
    )
}

/// Entry point: Parses multiple top-level items (funcs/concepts/impls).
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(
        multispace0,
        many0(ws(alt((parse_func(), parse_concept(), parse_impl())))),
        multispace0,
    )
    .parse(input)
}
