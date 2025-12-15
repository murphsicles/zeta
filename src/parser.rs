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
    mut inner: impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> {
    move |input| {
        let (input, _) = multispace0(input)?;
        let (input, result) = inner.parse(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, result))
    }
}

/// Parses an identifier (alpha + alphanum).
fn parse_ident(input: &str) -> IResult<&str, String> {
    map(
        pair(alpha1, alphanumeric0),
        |(first, rest): (&str, &str)| first.to_string() + rest,
    )
    .parse(input)
}

/// Parses keyword.
#[allow(dead_code)]
fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw))).parse(input)
}

/// Parses an integer literal.
fn parse_literal(input: &str) -> IResult<&str, AstNode> {
    map(nom_i64, AstNode::Lit).parse(input)
}

/// Parses string literal (r#"..."# stub as "..." for now).
fn parse_string_lit(input: &str) -> IResult<&str, AstNode> {
    map(
        delimited(tag("\""), take_while1(|c| c != '"'), tag("\"")),
        |s: &str| AstNode::StringLit(s.to_string()),
    )
    .parse(input)
}

/// Parses f-string content: recursive "text {expr} text".
fn parse_fstring_content(input: &str) -> IResult<&str, Vec<AstNode>> {
    let mut i = input;
    let mut parts = vec![];
    if !i.starts_with("f\"") {
        return Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Tag,
        )));
    }
    i = &i[2..];
    while !i.is_empty() && !i.starts_with('"') {
        if i.starts_with('{') {
            if let Some(closing) = i.find('}') {
                let expr_str = &i[1..closing];
                let (_, expr) = parse_full_expr(expr_str)?;
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
fn parse_fstring(input: &str) -> IResult<&str, AstNode> {
    map(parse_fstring_content, AstNode::FString).parse(input)
}

/// Parses a variable reference.
fn parse_variable(input: &str) -> IResult<&str, AstNode> {
    map(parse_ident, AstNode::Var).parse(input)
}

/// Parses path: A::B.
#[allow(dead_code)]
fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    map(
        many1(preceded(opt(tag("::")), parse_ident)),
        |ids: Vec<String>| ids,
    )
    .parse(input)
}

/// Parses atom: lit | var | str | fstr.
fn parse_atom(input: &str) -> IResult<&str, AstNode> {
    alt((parse_literal, parse_string_lit, parse_fstring, parse_variable)).parse(input)
}

/// Parses TimingOwned<ty>(atom).
fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("TimingOwned")).parse(input)?;
    let (input, ty) = delimited(tag("<"), parse_ident, tag(">")).parse(input)?;
    let (input, _) = tag("(").parse(input)?;
    let (input, inner) = parse_atom(input)?;
    let (input, _) = tag(")").parse(input)?;
    Ok((
        input,
        AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        },
    ))
}

/// Parses base expression: atom | TimingOwned.
fn parse_base_expr(input: &str) -> IResult<&str, AstNode> {
    alt((parse_atom, parse_timing_owned)).parse(input)
}

/// Parses parens: (expr).
fn parse_parens(input: &str) -> IResult<&str, AstNode> {
    delimited(tag("("), parse_expr, tag(")")).parse(input)
}

/// Parses expr recursively: base | parens.
fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    alt((parse_base_expr, parse_parens)).parse(input)
}

/// Parses structural marker: ? after method name.
fn parse_structural(input: &str) -> IResult<&str, bool> {
    map(opt(nom_char('?')), |opt_q| opt_q.is_some()).parse(input)
}

/// Parses type args: <T,U>.
fn parse_type_args(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tag("<"), separated_list1(tag(","), ws(parse_ident)), tag(">")).parse(input)
}

/// Parses call: recv.method<type_args>(args)?.
fn parse_call(input: &str) -> IResult<&str, AstNode> {
    let (input, recv) = opt(parse_expr).parse(input)?;
    let (input, method) = ws(parse_ident).parse(input)?;
    let (input, targs) = opt(parse_type_args).parse(input)?;
    let (input, args) =
        delimited(tag("("), separated_list1(tag(","), ws(parse_expr)), tag(")")).parse(input)?;
    let (input, _) = parse_structural(input)?;
    Ok((
        input,
        AstNode::Call {
            receiver: recv.map(Box::new),
            method,
            args,
            type_args: targs.unwrap_or_default(),
            structural: false, // From ?
        },
    ))
}

/// Parses binary: left + right (sugar for concat if str).
fn parse_binary(input: &str) -> IResult<&str, AstNode> {
    let (input, left) = parse_expr(input)?;
    let (input, op) = ws(alt((tag("+"), tag("-"), tag("*")))).parse(input)?;
    let (input, right) = parse_expr(input)?;
    Ok((
        input,
        AstNode::BinaryOp {
            op: op.to_string(), // + -> "concat" in resolver
            left: Box::new(left),
            right: Box::new(right),
        },
    ))
}

/// Full expr: binary | call | base.
fn parse_full_expr(input: &str) -> IResult<&str, AstNode> {
    alt((parse_binary, parse_call, parse_expr)).parse(input)
}

/// Parses assign: ident = expr.
fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    let (input, lhs) = ws(parse_ident).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, rhs) = ws(parse_full_expr).parse(input)?;
    Ok((input, AstNode::Assign(lhs, Box::new(rhs))))
}

/// Parses defer: defer expr.
fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("defer")).parse(input)?;
    let (input, expr) = ws(parse_full_expr).parse(input)?;
    Ok((input, AstNode::Defer(Box::new(expr))))
}

/// Parses spawn: spawn func(args).
fn parse_spawn(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("spawn")).parse(input)?;
    let (input, func) = ws(parse_ident).parse(input)?;
    let (input, args) = delimited(
        tag("("),
        separated_list1(tag(","), ws(parse_full_expr)),
        tag(")"),
    )
    .parse(input)?;
    Ok((input, AstNode::Spawn { func, args }))
}

/// Parses stmt: assign | expr | defer | spawn.
fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((parse_assign, parse_defer, parse_spawn, parse_full_expr)).parse(input)
}

/// Parses generics: <T,U>.
fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    parse_type_args(input)
}

/// Parses fn: fn name<gens>(params:Type) -> Ret { body }.
fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("fn"))).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generics)).parse(input)?;
    let (input, params) = delimited(
        tag("("),
        many0(pair(parse_ident, preceded(ws(tag(":")), ws(parse_ident)))),
        tag(")"),
    )
    .parse(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_ident))).parse(input)?;
    let (input, body) = delimited(ws(tag("{")), many0(ws(parse_stmt)), ws(tag("}"))).parse(input)?;
    let generics = generics_opt.unwrap_or_default();
    let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
    Ok((
        input,
        AstNode::FuncDef {
            name,
            generics,
            params,
            ret,
            body,
            attrs: vec![],
            ret_expr: None,
        },
    ))
}

/// Parses method sig for concept/impl: name(params) -> ret, with optional generics.
fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
    let (input, name) = parse_ident(input)?;
    let (input, generics_opt) = opt(ws(parse_generics)).parse(input)?;
    let (input, params) = delimited(tag("("), many0(parse_ident), tag(")")).parse(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_ident))).parse(input)?;
    Ok((
        input,
        AstNode::Method {
            name,
            params: params
                .into_iter()
                .map(|p| (p.clone(), "i64".to_string()))
                .collect(),
            ret: ret_opt.unwrap_or_else(|| "i64".to_string()),
            generics: generics_opt.unwrap_or_default(), // New: generics in methods
        },
    ))
}

/// Parses concept: concept name { methods }.
fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("concept"))).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, body) =
        delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))).parse(input)?;
    Ok((input, AstNode::ConceptDef { name, methods: body }))
}

/// Parses impl: impl concept for ty { methods }.
fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("impl"))).parse(input)?;
    let (input, concept) = ws(parse_ident).parse(input)?;
    let (input, _) = value((), ws(tag("for"))).parse(input)?;
    let (input, ty) = ws(parse_ident).parse(input)?;
    let (input, body) =
        delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))).parse(input)?;
    Ok((
        input,
        AstNode::ImplBlock { concept, ty, body },
    ))
}

/// Parses enum: enum name { variants }.
fn parse_enum(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("enum"))).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, variants) =
        delimited(ws(tag("{")), many0(ws(parse_ident)), ws(tag("}"))).parse(input)?;
    Ok((input, AstNode::EnumDef { name, variants }))
}

/// Parses struct: struct name { fields }.
fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("struct"))).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, fields) = delimited(
        ws(tag("{")),
        many0(map(
            pair(ws(parse_ident), preceded(ws(tag(":")), ws(parse_ident))),
            |(n, t)| (n, t),
        )),
        ws(tag("}")),
    )
    .parse(input)?;
    Ok((input, AstNode::StructDef { name, fields }))
}

/// Entry point: Parses multiple top-level items.
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(ws(alt((
        parse_func,
        parse_concept,
        parse_impl,
        parse_enum,
        parse_struct,
    ))))
    .parse(input)
}
