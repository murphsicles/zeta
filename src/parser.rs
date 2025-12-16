// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.
//! Now with generics: fn name<T,U>(params) -> Ret { body }
//! Added hybrid traits: structural dispatch via method? (e.g., obj.add?(b) for ad-hoc structural lookup)
//! Added partial specialization: method::<T,U>(args) for type args in calls.
//! Updated Dec 13, 2025: Added f-string parsing (f"hello {expr}"); + as concat; f-string lowering.
//! Updated Dec 16, 2025: Added parsing for single-line fns (fn name = expr;), ? prop, dict literals {k:v}, subscripts [index], return stmt; updated assign for complex lhs.

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{
    alpha1, alphanumeric0, i64 as nom_i64, multispace0,
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
fn ws<'a, F, O>(inner: F) -> impl Parser<&'a str, O, nom::error::Error<&'a str>>
where
    F: Parser<&'a str, O, nom::error::Error<&'a str>>,
{
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
            let text_end = i.find(['{', '"']).unwrap_or(i.len());
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

/// Parses dict literal: {k: v, ...}.
fn parse_dict_lit(input: &str) -> IResult<&str, AstNode> {
    map(delimited(ws(tag("{")), separated_list1(ws(tag(",")), pair(ws(parse_full_expr), preceded(ws(tag(":")), ws(parse_full_expr)))), ws(tag("}"))), |entries| AstNode::DictLit { entries }).parse(input)
}

/// Primary atoms: lit | var | str | fstr | dict | (expr).
fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_literal,
        parse_string_lit,
        parse_fstring,
        parse_variable,
        parse_dict_lit,
        delimited(ws(tag("(")), ws(parse_full_expr), ws(tag(")"))),
    )).parse(input)
}

/// Parses postfix operators: [index] or ? , with chaining.
fn parse_postfix_expr(input: &str) -> IResult<&str, AstNode> {
    let (mut input, mut base) = parse_primary(input)?;
    loop {
        if let Ok((i, _)) = ws(tag("?"))(input) {
            base = AstNode::TryProp { expr: Box::new(base) };
            input = i;
        } else if let Ok((i, index)) = delimited(ws(tag("[")), ws(parse_full_expr), ws(tag("]")))(input) {
            base = AstNode::Subscript { base: Box::new(base), index: Box::new(index) };
            input = i;
        } else if let Ok((i, method)) = preceded(ws(tag(".")), ws(parse_ident))(input) {
            let (i, type_args) = opt(ws(parse_generics))(i)?;
            let (i, structural) = if let Ok((i, _)) = ws(tag("?"))(i) { (i, true) } else { (i, false) };
            let (i, args) = delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(parse_full_expr)), ws(tag(")")))(i)?;
            base = AstNode::Call {
                receiver: Some(Box::new(base)),
                method,
                args,
                type_args: type_args.unwrap_or_default(),
                structural,
            };
            input = i;
        } else {
            break;
        }
    }
    Ok((input, base))
}

/// Parses binary op: postfix + postfix for concat, etc.
fn parse_binary_op(input: &str) -> IResult<&str, AstNode> {
    let (input, left) = parse_postfix_expr(input)?;
    let (input, pairs) = many0(pair(alt((ws(tag("+")), ws(tag("concat")))), parse_postfix_expr))(input)?;
    let mut expr = left;
    for (op, right) in pairs {
        expr = AstNode::BinaryOp { op: op.to_string(), left: Box::new(expr), right: Box::new(right) };
    }
    Ok((input, expr))
}

/// Parses free function call: name(args).
fn parse_free_call(input: &str) -> IResult<&str, AstNode> {
    let (input, method) = ws(parse_ident)(input)?;
    let (input, type_args) = opt(ws(parse_generics))(input)?;
    let (input, structural) = if let Ok((input, _)) = ws(tag("?"))(input) { (input, true) } else { (input, false) };
    let (input, args) = delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(parse_full_expr)), ws(tag(")")))(input)?;
    Ok((input, AstNode::Call {
        receiver: None,
        method,
        args,
        type_args: type_args.unwrap_or_default(),
        structural,
    }))
}

/// Parses full expr: call | binary | postfix | etc.
fn parse_full_expr(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_free_call,
        parse_binary_op,
        parse_timing_owned,
        parse_postfix_expr,
    ))(input)
}

/// Parses TimingOwned<ty>(atom).
fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("TimingOwned")).parse(input)?;
    let (input, ty) = delimited(tag("<"), parse_ident, tag(">")).parse(input)?;
    let (input, _) = tag("(").parse(input)?;
    let (input, inner) = parse_full_expr(input)?;
    let (input, _) = tag(")").parse(input)?;
    Ok((input, AstNode::TimingOwned { ty, inner: Box::new(inner) }))
}

/// Parses defer expr.
fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("defer")).parse(input)?;
    let (input, inner) = parse_full_expr(input)?;
    Ok((input, AstNode::Defer(Box::new(inner))))
}

/// Parses spawn func(args).
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

/// Parses return expr.
fn parse_return(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("return")).parse(input)?;
    let (input, expr) = ws(parse_full_expr)(input)?;
    Ok((input, AstNode::Return(Box::new(expr))))
}

/// Parses stmt: assign | defer | spawn | return | expr.
fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((parse_assign, parse_defer, parse_spawn, parse_return, parse_full_expr)).parse(input)
}

/// Parses assign: lhs = rhs.
fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    let (input, lhs) = ws(parse_postfix_expr)(input)?;
    let (input, _) = ws(tag("="))(input)?;
    let (input, rhs) = ws(parse_full_expr)(input)?;
    Ok((input, AstNode::Assign(Box::new(lhs), Box::new(rhs))))
}

/// Parses generics: <T,U>.
fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    delimited(ws(tag("<")), separated_list1(ws(tag(",")), ws(parse_ident)), ws(tag(">")))(input)
}

/// Parses fn: fn name<gens>(params) -> Ret { body } or = stmt for single-line.
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
    let (input, (body, single_line)) = alt((
        map(delimited(ws(tag("{")), many0(ws(parse_stmt)), ws(tag("}"))), |b| (b, false)),
        map(preceded(ws(tag("=")), ws(parse_stmt)), |s| (vec![s], true)),
    ))(input)?;
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
            single_line,
        },
    ))
}

/// Parses method sig for concept/impl: name(params) -> ret, with optional generics.
pub fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
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
    Ok((
        input,
        AstNode::ConceptDef {
            name,
            methods: body,
        },
    ))
}

/// Parses impl: impl concept for ty { methods }.
fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("impl"))).parse(input)?;
    let (input, concept) = ws(parse_ident).parse(input)?;
    let (input, _) = value((), ws(tag("for"))).parse(input)?;
    let (input, ty) = ws(parse_ident).parse(input)?;
    let (input, body) =
        delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))).parse(input)?;
    Ok((input, AstNode::ImplBlock { concept, ty, body }))
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
