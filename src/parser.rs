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
fn ws<'a, O, E: nom::error::ParseError<&'a str>>(
    inner: impl Parser<&'a str, O, E>,
) -> impl Parser<&'a str, O, E> {
    delimited(multispace0, inner, multispace0)
}

/// Parses an identifier (alpha + alphanum).
fn parse_ident(input: &str) -> IResult<&str, String> {
    map(alpha1.and(alphanumeric0), |(first, rest): (&str, &str)| {
        first.to_string() + rest
    })(input)
}

/// Parses keyword.
fn parse_keyword(input: &str, kw: &'static str) -> IResult<&str, ()> {
    value((), ws(tag(kw)))(input)
}

/// Parses an integer literal.
fn parse_literal(input: &str) -> IResult<&str, AstNode> {
    map(nom_i64, AstNode::Lit)(input)
}

/// Parses string literal (r#"..."# stub as "..." for now).
fn parse_string_lit(input: &str) -> IResult<&str, AstNode> {
    map(delimited(tag("\""), take_while1(|c| c != '"'), tag("\"")), |s: &str| AstNode::StringLit(s.to_string()))(input)
}

/// Parses a variable reference.
fn parse_variable(input: &str) -> IResult<&str, AstNode> {
    map(parse_ident, AstNode::Var)(input)
}

/// Parses path: A::B.
fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    map(many1(preceded(opt(tag("::")), parse_ident)), |ids: Vec<String>| ids)(input)
}

/// Parses base expression: lit | var | str | TimingOwned<ty>(inner).
fn parse_base_expr(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_literal,
        parse_string_lit,
        parse_variable,
        parse_timing_owned,
    ))(input)
}

fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    map(
        (
            ws(tag("TimingOwned")),
            delimited(tag("<"), parse_ident, tag(">")),
            tag("("),
            parse_expr,
            tag(")"),
        ),
        |(_, ty, _, inner, _)| AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        },
    )(input)
}

/// Parses add: base + base (as binary add).
fn parse_add(input: &str) -> IResult<&str, ((), AstNode)> {
    ws(tag("+")).and(parse_base_expr)(input)
}

/// Parses call: (args) base.
fn parse_call(input: &str) -> IResult<&str, AstNode> {
    map(
        delimited(
            tag("("),
            many0(ws(parse_expr)),
            tag(")"),
        )
        .and(parse_base_expr),
        |(args, recv)| AstNode::Call {
            receiver: Some(Box::new(recv)),
            method: "call".to_string(),
            args,
            type_args: vec![],
        },
    )(input)
}

/// Parses path call: path :: method (args).
fn parse_path_call(input: &str) -> IResult<&str, AstNode> {
    map(
        (parse_path, ws(tag("::")), parse_ident, delimited(tag("("), many0(ws(parse_expr)), tag(")"))),
        |(path, _, method, args)| AstNode::PathCall {
            path,
            method,
            args,
        },
    )(input)
}

/// Parses expression: path_call | call | base [+ base].
fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    let base_or_add = parse_base_expr.and(opt(parse_add)).map(|(left, opt_add)| {
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

    alt((parse_path_call, parse_call, base_or_add))(input)
}

/// Parses method signature: name(params) -> ret.
fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
    let parse_param_pair = ws(parse_ident)
        .and(ws(tag(":")))
        .and(ws(parse_ident))
        .map(|((n, _), t): ((String, &str), String)| (n, t));

    let parse_params = delimited(tag("("), many0(parse_param_pair), tag(")"));

    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_ident)));

    map(
        (parse_ident, parse_params, parse_ret),
        |(name, params, ret_opt)| {
            let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
            AstNode::Method {
                name,
                params,
                ret,
            }
        },
    )(input)
}

/// Parses assign: ident = expr;.
fn parse_assign_stmt(input: &str) -> IResult<&str, AstNode> {
    map(
        (ws(parse_ident), ws(tag("=")), ws(parse_expr), ws(tag(";"))),
        |(name, _, expr, _)| AstNode::Assign(name, Box::new(expr)),
    )(input)
}

/// Parses defer: defer expr;.
fn parse_defer_stmt(input: &str) -> IResult<&str, AstNode> {
    map(
        (ws(tag("defer")), ws(parse_expr), ws(tag(";"))),
        |(_, e, _)| AstNode::Defer(Box::new(e)),
    )(input)
}

/// Parses spawn: spawn ident (args);.
fn parse_spawn_stmt(input: &str) -> IResult<&str, AstNode> {
    let parse_kw = ws(tag("spawn"));
    let parse_func = parse_ident;
    let parse_args = delimited(tag("("), many0(ws(parse_expr)), tag(")"));

    map(
        (parse_kw, parse_func, parse_args, ws(tag(";"))),
        |(_, func, args, _)| AstNode::Spawn { func, args },
    )(input)
}

/// Parses statement: assign | spawn | defer | expr;.
fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_assign_stmt,
        parse_spawn_stmt,
        parse_defer_stmt,
        terminated(parse_expr, ws(tag(";"))),
    ))(input)
}

/// Parses function body: { stmt* }.
fn parse_func_body(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(ws(tag("{")), many0(ws(parse_stmt)), ws(tag("}")))(input)
}

/// Parses function: fn name (params) -> ret { body }.
fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let parse_fn_kw = ws(tag("fn"));
    let parse_name = parse_ident;
    let parse_param_pair = ws(parse_ident)
        .and(ws(tag(":")))
        .and(ws(parse_ident))
        .map(|((n, _), t): ((String, &str), String)| (n, t));
    let parse_params = delimited(tag("("), many0(parse_param_pair), tag(")"));
    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_ident)));
    let parse_body = parse_func_body;

    map(
        (parse_fn_kw, parse_name, parse_params, parse_ret, parse_body),
        |(_, name, params, ret_opt, body)| {
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
    )(input)
}

/// Parses concept: concept name { methods }.
fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let parse_kw = ws(tag("concept"));
    let parse_name = parse_ident;
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}")));

    map((parse_kw, parse_name, parse_body), |(_, name, body)| AstNode::ConceptDef {
        name,
        methods: body,
    })(input)
}

/// Parses impl: impl concept for ty { methods }.
fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let parse_kw = ws(tag("impl"));
    let parse_concept = parse_ident;
    let parse_for_kw = ws(tag("for"));
    let parse_ty = parse_ident;
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}")));

    map(
        (parse_kw, parse_concept, parse_for_kw, parse_ty, parse_body),
        |(_, concept, _, ty, body)| AstNode::ImplBlock { concept, ty, body },
    )(input)
}

/// Parses enum: enum name { variants }.
fn parse_enum(input: &str) -> IResult<&str, AstNode> {
    let parse_kw = ws(tag("enum"));
    let parse_name = parse_ident;
    let parse_variants = delimited(ws(tag("{")), many0(ws(parse_ident)), ws(tag("}")));

    map((parse_kw, parse_name, parse_variants), |(_, name, variants)| AstNode::EnumDef {
        name,
        variants,
    })(input)
}

/// Parses struct: struct name { fields }.
fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    let parse_kw = ws(tag("struct"));
    let parse_name = parse_ident;
    let parse_field = ws(parse_ident)
        .and(ws(tag(":")))
        .and(ws(parse_ident))
        .map(|((n, _), t): ((String, &str), String)| (n, t));
    let parse_fields = delimited(ws(tag("{")), many0(parse_field), ws(tag("}")));

    map((parse_kw, parse_name, parse_fields), |(_, name, fields)| AstNode::StructDef {
        name,
        fields,
    })(input)
}

/// Entry point: Parses multiple top-level items (funcs/concepts/impls/enums/structs).
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(ws(alt((
        parse_func,
        parse_concept,
        parse_impl,
        parse_enum,
        parse_struct,
    ))))(input)
}
