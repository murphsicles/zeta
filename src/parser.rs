// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, alphanumeric0, i64 as nom_i64, multispace0};
use nom::combinator::{map, opt, value};
use nom::multi::{many0, many1};
use nom::sequence::{delimited, preceded, terminated};
use nom::{IResult, Parser};

/// Whitespace wrapper for parsers.
fn ws<'a, O>(
    inner: impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

/// Parses an identifier (alpha + alphanum).
fn parse_ident<'a>() -> impl Parser<&'a str, Output = String, Error = nom::error::Error<&'a str>> {
    map(alpha1.and(alphanumeric0), |(first, rest): (&str, &str)| {
        first.to_string() + rest
    })
}

/// Parses keyword.
fn parse_keyword<'a>(
    kw: &'static str,
) -> impl Parser<&'a str, Output = (), Error = nom::error::Error<&'a str>> {
    value((), ws(tag(kw)))
}

/// Parses an integer literal.
fn parse_literal<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    map(nom_i64, AstNode::Lit)
}

/// Parses string literal (r#"..."# stub as "..." for now).
fn parse_string_lit<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        delimited(tag("\""), take_while1(|c| c != '"'), tag("\"")),
        |s: &str| AstNode::StringLit(s.to_string()),
    )
}

/// Parses a variable reference.
fn parse_variable<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    map(parse_ident(), AstNode::Var)
}

/// Parses path: A::B.
fn parse_path<'a>() -> impl Parser<&'a str, Output = Vec<String>, Error = nom::error::Error<&'a str>>
{
    map(
        many1(preceded(opt(tag("::")), parse_ident())),
        |ids: Vec<String>| ids,
    )
}

/// Parses TimingOwned<ty>(inner).
fn parse_timing_owned<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        (
            ws(tag("TimingOwned")),
            delimited(tag("<"), parse_ident(), tag(">")),
            tag("("),
            parse_expr(),
            tag(")"),
        ),
        |(_, ty, _, inner, _)| AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        },
    )
}

/// Parses base expression: lit | var | str | TimingOwned.
fn parse_base_expr<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((
        parse_literal(),
        parse_string_lit(),
        parse_variable(),
        parse_timing_owned(),
    ))
}

/// Parses add: base + base (as binary add).
fn parse_add<'a>()
-> impl Parser<&'a str, Output = ((), AstNode), Error = nom::error::Error<&'a str>> {
    value((), ws(tag("+"))).and(parse_base_expr())
}

/// Parses call: (args) base.
fn parse_call<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        delimited(tag("("), many0(ws(parse_expr())), tag(")")).and(parse_base_expr()),
        |(args, recv)| AstNode::Call {
            receiver: Some(Box::new(recv)),
            method: "call".to_string(),
            args,
            type_args: vec![],
        },
    )
}

/// Parses path call: path :: method (args).
fn parse_path_call<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        (
            parse_path(),
            ws(tag("::")),
            parse_ident(),
            delimited(tag("("), many0(ws(parse_expr())), tag(")")),
        ),
        |(path, _, method, args)| AstNode::PathCall { path, method, args },
    )
}

/// Parses expression recursively.
fn parse_expr<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let base_or_add = parse_base_expr()
        .and(opt(parse_add()))
        .map(|(left, opt_add)| {
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
        });
    alt((parse_path_call(), parse_call(), base_or_add))
}

/// Parses defer: defer expr;.
fn parse_defer_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        preceded(ws(tag("defer")), terminated(parse_expr(), ws(tag(";")))),
        |e| AstNode::Defer(Box::new(e)),
    )
}

/// Parses assign: let var = expr;.
fn parse_assign_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        (
            ws(tag("let")),
            parse_ident(),
            ws(tag("=")),
            parse_expr(),
            ws(tag(";")),
        ),
        |(_, name, _, expr, _)| AstNode::Assign(name, Box::new(expr)),
    )
}

/// Parses spawn: spawn func(args);.
fn parse_spawn_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("spawn")));
    let parse_func = parse_ident();
    let parse_args = delimited(tag("("), many0(ws(parse_expr())), tag(")"));

    map(
        (parse_kw, parse_func, parse_args, ws(tag(";"))),
        |(_, func, args, _)| AstNode::Spawn { func, args },
    )
}

/// Parses statement: assign | spawn | defer | expr;.
fn parse_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((
        parse_assign_stmt(),
        parse_spawn_stmt(),
        parse_defer_stmt(),
        terminated(parse_expr(), ws(tag(";"))),
    ))
}

/// Parses function body: { stmt* }.
fn parse_func_body<'a>()
-> impl Parser<&'a str, Output = Vec<AstNode>, Error = nom::error::Error<&'a str>> {
    delimited(ws(tag("{")), many0(ws(parse_stmt())), ws(tag("}")))
}

/// Parses function: fn name (params) -> ret { body }.
fn parse_func<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_fn_kw = value((), ws(tag("fn")));
    let parse_name = parse_ident();
    let parse_param_pair = ws(parse_ident())
        .and(ws(tag(":")))
        .and(ws(parse_ident()))
        .map(|((n, _), t): ((String, &'a str), String)| (n, t));
    let parse_params = delimited(tag("("), many0(parse_param_pair), tag(")"));
    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_ident())));
    let parse_body = parse_func_body();

    map(
        (parse_fn_kw, parse_name, parse_params, parse_ret, parse_body),
        |(_, name, params, ret_opt, body): (
            (),
            String,
            Vec<(String, String)>,
            Option<String>,
            Vec<AstNode>,
        )| {
            let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
            AstNode::FuncDef {
                name,
                generics: vec![],
                params,
                ret,
                body,
                attrs: vec![],
                ret_expr: None,
            }
        },
    )
}

/// Parses method sig for concept/impl.
fn parse_method_sig<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_name = parse_ident();
    let parse_params = delimited(tag("("), many0(parse_ident()), tag(")"));
    let parse_ret = preceded(ws(tag("->")), ws(parse_ident()));

    map(
        (parse_name, parse_params, parse_ret),
        | (name, params, ret) | AstNode::Method {
            name,
            params: params.into_iter().map(|p| (p.clone(), "i64".to_string())).collect(),
            ret,
        }
    )
}

/// Parses concept: concept name { methods }.
fn parse_concept<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    let parse_kw = value((), ws(tag("concept")));
    let parse_name = parse_ident();
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig())), ws(tag("}")));

    map(
        (parse_kw, parse_name, parse_body),
        |(_, name, body): ((), String, Vec<AstNode>)| AstNode::ConceptDef {
            name,
            methods: body,
        },
    )
}

/// Parses impl: impl concept for ty { methods }.
fn parse_impl<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("impl")));
    let parse_concept = parse_ident();
    let parse_for_kw = value((), ws(tag("for")));
    let parse_ty = parse_ident();
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig())), ws(tag("}")));

    map(
        (parse_kw, parse_concept, parse_for_kw, parse_ty, parse_body),
        |(_, concept, _, ty, body): ((), String, (), String, Vec<AstNode>)| AstNode::ImplBlock {
            concept,
            ty,
            body,
        },
    )
}

/// Parses enum: enum name { variants }.
fn parse_enum<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("enum")));
    let parse_name = parse_ident();
    let parse_variants = delimited(ws(tag("{")), many0(ws(parse_ident())), ws(tag("}")));

    map(
        (parse_kw, parse_name, parse_variants),
        |(_, name, variants): ((), String, Vec<String>)| AstNode::EnumDef { name, variants },
    )
}

/// Parses struct: struct name { fields }.
fn parse_struct<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    let parse_kw = value((), ws(tag("struct")));
    let parse_name = parse_ident();
    let parse_field = ws(parse_ident())
        .and(ws(tag(":")))
        .and(ws(parse_ident()))
        .map(|((n, _), t): ((String, &'a str), String)| (n, t));
    let parse_fields = delimited(ws(tag("{")), many0(parse_field), ws(tag("}")));

    map(
        (parse_kw, parse_name, parse_fields),
        |(_, name, fields): ((), String, Vec<(String, String)>)| AstNode::StructDef {
            name,
            fields,
        },
    )
}

/// Entry point: Parses multiple top-level items (funcs/concepts/impls/enums/structs).
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(ws(alt((
        parse_func(),
        parse_concept(),
        parse_impl(),
        parse_enum(),
        parse_struct(),
    ))))(input)
}
