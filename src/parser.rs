// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.
//! Now with generics: fn name<T,U>(params) -> Ret { body }
//! Added hybrid traits: structural dispatch via method? (e.g., obj.add?(b) for ad-hoc structural lookup)
//! Added partial specialization: method::<T,U>(args) for type args in calls.
//! Updated Dec 13, 2025: Added f-string parsing (f"hello {expr}"); + as concat; f-string lowering.

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{
    alpha1, alphanumeric0, char as nom_char, i64 as nom_i64, multispace0,
};
use nom::combinator::{map, opt, value};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};

#[allow(dead_code)]
type FnParse = (
    (),
    String,
    Vec<String>,
    Vec<(String, String)>,
    Option<String>,
    Vec<AstNode>,
);

/// Whitespace wrapper for parsers.
fn ws<'a, O>(
    inner: impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

/// Parses an identifier (alpha + alphanum).
fn parse_ident<'a>(input: &'a str) -> IResult<&'a str, String> {
    map(
        pair(alpha1, alphanumeric0),
        |(first, rest): (&str, &str)| first.to_string() + rest,
    )(input)
}

/// Parses keyword.
#[allow(dead_code)]
fn parse_keyword<'a>(kw: &'static str) -> impl Parser<&'a str, Output = (), Error = nom::error::Error<&'a str>> {
    value((), ws(tag(kw)))
}

/// Parses an integer literal.
fn parse_literal<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(nom_i64, AstNode::Lit)(input)
}

/// Parses string literal (r#"..."# stub as "..." for now).
fn parse_string_lit<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        delimited(tag("\""), take_while1(|c| c != '"'), tag("\"")),
        |s: &str| AstNode::StringLit(s.to_string()),
    )(input)
}

/// Parses f-string content: recursive "text {expr} text".
fn parse_fstring_content(input: &str) -> IResult<&str, Vec<AstNode>> {
    let mut i = input;
    let mut parts = vec![];
    if !i.starts_with("f\"") {
        return Err(nom::Err::Error(nom::error::Error::new(i, nom::error::ErrorKind::Tag)));
    }
    i = &i[2..];
    while !i.is_empty() && !i.starts_with('"') {
        if i.starts_with('{') {
            if let Some(closing) = i.find('}') {
                let expr_str = &i[1..closing];
                let (rest, expr) = parse_full_expr(expr_str)?;
                parts.push(expr);
                i = &i[closing + 1..];
            } else {
                break;
            }
        } else {
            let text_end = i.find(|c| c == '{' || c == '"').unwrap_or(i.len());
            if text_end > 0 {
                let text = &i[..text_end];
                parts.push(AstNode::StringLit(text.to_string()));
            }
            i = &i[text_end..];
        }
    }
    if i.starts_with('"') {
        i = &i[1..];
    }
    Ok((i, parts))
}

/// Parses f-string: f"content {expr} more".
fn parse_fstring<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(parse_fstring_content, AstNode::FString)(input)
}

/// Parses a variable reference.
fn parse_variable<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(parse_ident, AstNode::Var)(input)
}

/// Parses path: A::B.
#[allow(dead_code)]
fn parse_path<'a>(input: &'a str) -> IResult<&'a str, Vec<String>> {
    map(
        many1(preceded(opt(tag("::")), parse_ident)),
        |ids: Vec<String>| ids,
    )(input)
}

/// Parses atom: lit | var | str | fstr.
fn parse_atom<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    alt((
        parse_literal,
        parse_string_lit,
        parse_fstring,
        parse_variable,
    ))(input)
}

/// Parses TimingOwned<ty>(atom).
fn parse_timing_owned<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            ws(tag("TimingOwned")),
            delimited(tag("<"), parse_ident, tag(">")),
            tag("("),
            parse_atom,
            tag(")"),
        )),
        |(_, ty, _, inner, _)| AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        },
    )(input)
}

/// Parses base expression: atom | TimingOwned.
fn parse_base_expr<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    alt((parse_atom, parse_timing_owned))(input)
}

/// Parses parens: (expr).
fn parse_parens<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    delimited(tag("("), parse_expr, tag(")"))(input)
}

/// Parses expr recursively: base | parens.
fn parse_expr<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    alt((parse_base_expr, parse_parens))(input)
}

/// Parses structural marker: ? after method name.
fn parse_structural<'a>(input: &'a str) -> IResult<&'a str, bool> {
    map(opt(nom_char('?')), |opt_q| opt_q.is_some())(input)
}

/// Parses type args: <T,U>.
fn parse_type_args<'a>(input: &'a str) -> IResult<&'a str, Vec<String>> {
    delimited(
        tag("<"),
        separated_list1(tag(","), ws(parse_ident)),
        tag(">"),
    )(input)
}

/// Parses call: recv.method<type_args>(args)?.
fn parse_call<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            opt(parse_expr),
            ws(parse_ident),
            opt(parse_type_args),
            delimited(tag("("), separated_list1(tag(","), ws(parse_expr)), tag(")")),
            parse_structural,
        )),
        |(recv, method, targs, args, _)| AstNode::Call {
            receiver: recv.map(Box::new),
            method,
            args,
            type_args: targs.unwrap_or(vec![]),
            structural: false, // From ?
        },
    )(input)
}

/// Parses binary: left + right (sugar for concat if str).
fn parse_binary<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((parse_expr, ws(alt((tag("+"), tag("-"), tag("*")))), parse_expr)),
        |(left, op, right)| AstNode::BinaryOp {
            op: op.to_string(), // + -> "concat" in resolver
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

/// Full expr: binary | call | base.
fn parse_full_expr<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    alt((parse_binary, parse_call, parse_expr))(input)
}

/// Parses assign: ident = expr.
fn parse_assign<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((ws(parse_ident), ws(tag("=")), ws(parse_full_expr))),
        |(lhs, _, rhs)| AstNode::Assign(lhs, Box::new(rhs)),
    )(input)
}

/// Parses defer: defer expr.
fn parse_defer<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(preceded(ws(tag("defer")), ws(parse_full_expr)), |expr| {
        AstNode::Defer(Box::new(expr))
    })(input)
}

/// Parses spawn: spawn func(args).
fn parse_spawn<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            ws(tag("spawn")),
            ws(parse_ident),
            delimited(tag("("), separated_list1(tag(","), ws(parse_full_expr)), tag(")")),
        )),
        |(_, func, args)| AstNode::Spawn { func, args },
    )(input)
}

/// Parses stmt: assign | expr | defer | spawn.
fn parse_stmt<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    alt((parse_assign, parse_defer, parse_spawn, parse_full_expr))(input)
}

/// Parses generics: <T,U>.
fn parse_generics<'a>(input: &'a str) -> IResult<&'a str, Vec<String>> {
    parse_type_args(input)
}

/// Parses fn: fn name<gens>(params:Type) -> Ret { body }.
fn parse_func<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            value((), ws(tag("fn"))),
            ws(parse_ident),
            opt(ws(parse_generics)),
            delimited(tag("("), many0(map(tuple((parse_ident, preceded(ws(tag(":")), ws(parse_ident)))), |(p, t)| (p, t))), tag(")")),
            opt(preceded(ws(tag("->")), ws(parse_ident))),
            delimited(ws(tag("{")), many0(ws(parse_stmt)), ws(tag("}"))),
        )),
        |(_, name, generics_opt, params, ret_opt, body)| {
            let generics = generics_opt.unwrap_or(vec![]);
            let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
            AstNode::FuncDef {
                name,
                generics,
                params,
                ret,
                body,
                attrs: vec![],
                ret_expr: None,
            }
        },
    )(input)
}

/// Parses method sig for concept/impl: name(params) -> ret, with optional generics.
fn parse_method_sig<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            parse_ident,
            opt(ws(parse_generics)),
            delimited(tag("("), many0(parse_ident), tag(")")),
            opt(preceded(ws(tag("->")), ws(parse_ident))),
        )),
        |(name, generics_opt, params, ret_opt)| AstNode::Method {
            name,
            params: params
                .into_iter()
                .map(|p| (p.clone(), "i64".to_string()))
                .collect(),
            ret: ret_opt.unwrap_or_else(|| "i64".to_string()),
            generics: generics_opt.unwrap_or(vec![]), // New: generics in methods
        },
    )(input)
}

/// Parses concept: concept name { methods }.
fn parse_concept<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            value((), ws(tag("concept"))),
            ws(parse_ident),
            delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))),
        )),
        |(_, name, body)| AstNode::ConceptDef { name, methods: body },
    )(input)
}

/// Parses impl: impl concept for ty { methods }.
fn parse_impl<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            value((), ws(tag("impl"))),
            ws(parse_ident),
            value((), ws(tag("for"))),
            ws(parse_ident),
            delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))),
        )),
        |(_, concept, _, ty, body)| AstNode::ImplBlock { concept, ty, body },
    )(input)
}

/// Parses enum: enum name { variants }.
fn parse_enum<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            value((), ws(tag("enum"))),
            ws(parse_ident),
            delimited(ws(tag("{")), many0(ws(parse_ident)), ws(tag("}"))),
        )),
        |(_, name, variants)| AstNode::EnumDef { name, variants },
    )(input)
}

/// Parses struct: struct name { fields }.
fn parse_struct<'a>(input: &'a str) -> IResult<&'a str, AstNode> {
    map(
        tuple((
            value((), ws(tag("struct"))),
            ws(parse_ident),
            delimited(
                ws(tag("{")),
                many0(map(
                    tuple((ws(parse_ident), ws(tag(":")), ws(parse_ident))),
                    |((n, _), t)| (n, t),
                )),
                ws(tag("}")),
            ),
        )),
        |(_, name, fields)| AstNode::StructDef { name, fields },
    )(input)
}

/// Entry point: Parses multiple top-level items.
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(ws(alt((
        parse_func,
        parse_concept,
        parse_impl,
        parse_enum,
        parse_struct,
    ))))(input)
}
