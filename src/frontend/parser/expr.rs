// src/frontend/parser/expr.rs
use crate::frontend::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::i64;
use nom::combinator::{map, opt};
use nom::multi::separated_list1;
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult};

use super::parser::{parse_ident, parse_path, parse_generics, ws};

fn parse_literal(input: &str) -> IResult<&str, AstNode> {
    map(i64, AstNode::Lit)(input)
}

fn parse_string_lit(input: &str) -> IResult<&str, AstNode> {
    map(
        delimited(tag("\""), take_while1(|c| c != '"'), tag("\"")),
        |s: &str| AstNode::StringLit(s.to_string()),
    )(input)
}

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
    Ok((i, parts))
}

fn parse_fstring(input: &str) -> IResult<&str, AstNode> {
    map(parse_fstring_content, AstNode::FString)(input)
}

fn parse_variable(input: &str) -> IResult<&str, AstNode> {
    map(parse_ident, AstNode::Var)(input)
}

fn parse_dict_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, entries) = delimited(
        ws(tag("{")),
        separated_list1(ws(tag(",")), pair(ws(parse_full_expr), preceded(ws(tag(":")), ws(parse_full_expr)))),
        ws(tag("}")),
    )(input)?;
    Ok((input, AstNode::DictLit { entries }))
}

fn parse_paren_expr(input: &str) -> IResult<&str, AstNode> {
    delimited(ws(tag("(")), ws(parse_full_expr), ws(tag(")")))(input)
}

fn parse_call(input: &str) -> IResult<&str, AstNode> {
    let (input, receiver_opt) = opt(ws(parse_primary_expr))(input)?;
    let (input, method) = preceded(ws(tag(".")), ws(parse_ident))(input)?;
    let (input, type_args_opt) = opt(ws(parse_generics))(input)?;
    let (input, args) = delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(parse_full_expr)), ws(tag(")")))(input)?;
    let type_args: Vec<String> = type_args_opt.unwrap_or_default();
    Ok((
        input,
        AstNode::Call {
            receiver: receiver_opt.map(Box::new),
            method,
            args,
            type_args,
            structural: false,
        },
    ))
}

fn parse_path_call(input: &str) -> IResult<&str, AstNode> {
    let (input, path) = ws(parse_path)(input)?;
    let (input, method) = preceded(ws(tag("::")), ws(parse_ident))(input)?;
    let (input, args) = delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(parse_full_expr)), ws(tag(")")))(input)?;
    Ok((input, AstNode::PathCall { path, method, args }))
}

fn parse_spawn(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("spawn"))(input)?;
    let (input, func) = ws(parse_ident)(input)?;
    let (input, args) = delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(parse_full_expr)), ws(tag(")")))(input)?;
    Ok((input, AstNode::Spawn { func, args }))
}

fn parse_binary_op(input: &str) -> IResult<&str, AstNode> {
    let (input, left) = ws(parse_primary_expr)(input)?;
    let (input, op) = ws(alt((tag("+"), tag("-"), tag("*"), tag("/"))))(input)?;
    let (input, right) = ws(parse_primary_expr)(input)?;
    Ok((input, AstNode::BinaryOp {
        op: op.to_string(),
        left: Box::new(left),
        right: Box::new(right),
    }))
}

fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("TimingOwned"))(input)?;
    let (input, ty) = ws(parse_ident)(input)?;
    let (input, inner) = ws(parse_full_expr)(input)?;
    Ok((input, AstNode::TimingOwned { ty, inner: Box::new(inner) }))
}

fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("defer"))(input)?;
    let (input, inner) = ws(parse_full_expr)(input)?;
    Ok((input, AstNode::Defer(Box::new(inner))))
}

fn parse_try_prop(input: &str) -> IResult<&str, AstNode> {
    let (input, expr) = ws(parse_full_expr)(input)?;
    let (input, _) = ws(tag("?"))(input)?;
    Ok((input, AstNode::TryProp { expr: Box::new(expr) }))
}

fn parse_subscript(input: &str) -> IResult<&str, AstNode> {
    let (input, base) = ws(parse_primary_expr)(input)?;
    let (input, index) = delimited(ws(tag("[")), ws(parse_full_expr), ws(tag("]")))(input)?;
    Ok((input, AstNode::Subscript { base: Box::new(base), index: Box::new(index) }))
}

fn parse_primary_expr(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_literal,
        parse_string_lit,
        parse_fstring,
        parse_variable,
        parse_dict_lit,
        parse_paren_expr,
        parse_call,
        parse_path_call,
        parse_spawn,
        parse_binary_op,
        parse_timing_owned,
        parse_defer,
        parse_try_prop,
        parse_subscript,
    ))(input)
}

pub fn parse_full_expr(input: &str) -> IResult<&str, AstNode> {
    parse_primary_expr(input)
}
