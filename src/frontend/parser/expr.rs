// src/frontend/parser/expr.rs
use super::parser::{parse_generics, parse_ident, parse_path, ws};
use crate::frontend::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::i64 as nom_i64;
use nom::combinator::opt;
use nom::multi::separated_list1;
use nom::sequence::delimited;

fn parse_literal(input: &str) -> IResult<&str, AstNode> {
    let (input, val) = nom_i64(input)?;
    Ok((input, AstNode::Lit(val)))
}

fn parse_string_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("\"")(input)?;
    let (input, s) = take_while1(|c| c != '"')(input)?;
    let (input, _) = tag("\"")(input)?;
    Ok((input, AstNode::StringLit(s.to_string())))
}

fn parse_fstring_content(input: &str) -> IResult<&str, Vec<AstNode>> {
    let mut parts = vec![];
    let mut remaining = input;

    if !remaining.starts_with("f\"") {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    remaining = &remaining[2..];

    loop {
        if remaining.is_empty() || remaining.starts_with('"') {
            break;
        }

        // Handle escaped braces
        if remaining.starts_with("{{") || remaining.starts_with("}}") {
            let escaped = &remaining[..2];
            parts.push(AstNode::StringLit(
                if escaped == "{{" { "{" } else { "}" }.to_string(),
            ));
            remaining = &remaining[2..];
            continue;
        }

        // Static text part
        let static_end = remaining.find(['{', '"']).unwrap_or(remaining.len());
        if static_end > 0 {
            let text = &remaining[..static_end];
            if !text.trim().is_empty() {
                parts.push(AstNode::StringLit(text.to_string()));
            }
            remaining = &remaining[static_end..];
        }

        // Interpolated expression {expr:format_spec}
        if remaining.starts_with('{') {
            if let Some(end) = remaining[1..].find('}') {
                let inner = &remaining[1..=end];
                let (_format_spec, expr_str) = if let Some(colon_pos) = inner.find(':') {
                    (&inner[colon_pos + 1..], &inner[..colon_pos])
                } else {
                    ("", inner)
                };
                let expr_str = expr_str.trim();
                if expr_str.is_empty() {
                    remaining = &remaining[end + 2..];
                    continue;
                }
                let (_, expr) = parse_full_expr(expr_str)?;
                parts.push(expr);
                remaining = &remaining[end + 2..];
            } else {
                break;
            }
        }
    }

    if remaining.starts_with('"') {
        remaining = &remaining[1..];
    }

    Ok((remaining, parts))
}

fn parse_fstring(input: &str) -> IResult<&str, AstNode> {
    let (input, parts) = parse_fstring_content(input)?;
    Ok((input, AstNode::FString(parts)))
}

fn parse_variable(input: &str) -> IResult<&str, AstNode> {
    let (input, name) = parse_ident(input)?;
    Ok((input, AstNode::Var(name)))
}

fn parse_dict_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("{")).parse(input)?;
    let (input, entries) = separated_list1(ws(tag(",")), |i| {
        let (i, key) = ws(parse_full_expr).parse(i)?;
        let (i, _) = ws(tag(":")).parse(i)?;
        let (i, val) = ws(parse_full_expr).parse(i)?;
        Ok((i, (key, val)))
    })
    .parse(input)?;
    let (input, _) = ws(tag("}")).parse(input)?;
    Ok((input, AstNode::DictLit { entries }))
}

fn parse_paren_expr(input: &str) -> IResult<&str, AstNode> {
    delimited(ws(tag("(")), ws(parse_full_expr), ws(tag(")"))).parse(input)
}

fn parse_call(input: &str) -> IResult<&str, AstNode> {
    let (input, receiver_opt) = opt(|i| {
        let (i, recv) = ws(parse_primary_expr).parse(i)?;
        let (i, _) = ws(tag(".")).parse(i)?;
        Ok((i, recv))
    })
    .parse(input)?;
    let (input, method) = ws(parse_ident).parse(input)?;
    let (input, type_args_opt) = opt(ws(parse_generics)).parse(input)?;
    let (input, args) = delimited(
        ws(tag("(")),
        separated_list1(ws(tag(",")), ws(parse_full_expr)),
        ws(tag(")")),
    )
    .parse(input)?;
    let type_args = type_args_opt.unwrap_or_default();
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
    let (input, path) = ws(parse_path).parse(input)?;
    let (input, _) = ws(tag("::")).parse(input)?;
    let (input, method) = ws(parse_ident).parse(input)?;
    let (input, args) = delimited(
        ws(tag("(")),
        separated_list1(ws(tag(",")), ws(parse_full_expr)),
        ws(tag(")")),
    )
    .parse(input)?;
    Ok((input, AstNode::PathCall { path, method, args }))
}

fn parse_spawn(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("spawn")).parse(input)?;
    let (input, func) = ws(parse_ident).parse(input)?;
    let (input, args) = delimited(
        ws(tag("(")),
        separated_list1(ws(tag(",")), ws(parse_full_expr)),
        ws(tag(")")),
    )
    .parse(input)?;
    Ok((input, AstNode::Spawn { func, args }))
}

fn parse_binary_op(input: &str) -> IResult<&str, AstNode> {
    let (input, left) = ws(parse_primary_expr).parse(input)?;
    let (input, op) = ws(alt((tag("+"), tag("-"), tag("*"), tag("/")))).parse(input)?;
    let (input, right) = ws(parse_primary_expr).parse(input)?;
    Ok((
        input,
        AstNode::BinaryOp {
            op: op.to_string(),
            left: Box::new(left),
            right: Box::new(right),
        },
    ))
}

fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("TimingOwned")).parse(input)?;
    let (input, ty) = ws(parse_ident).parse(input)?;
    let (input, inner) = ws(parse_full_expr).parse(input)?;
    Ok((
        input,
        AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        },
    ))
}

fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("defer")).parse(input)?;
    let (input, inner) = ws(parse_full_expr).parse(input)?;
    Ok((input, AstNode::Defer(Box::new(inner))))
}

fn parse_try_prop(input: &str) -> IResult<&str, AstNode> {
    let (input, expr) = ws(parse_full_expr).parse(input)?;
    let (input, _) = ws(tag("?")).parse(input)?;
    Ok((
        input,
        AstNode::TryProp {
            expr: Box::new(expr),
        },
    ))
}

fn parse_subscript(input: &str) -> IResult<&str, AstNode> {
    let (input, base) = ws(parse_primary_expr).parse(input)?;
    let (input, index) = delimited(ws(tag("[")), ws(parse_full_expr), ws(tag("]"))).parse(input)?;
    Ok((
        input,
        AstNode::Subscript {
            base: Box::new(base),
            index: Box::new(index),
        },
    ))
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
    ))
    .parse(input)
}

pub fn parse_full_expr(input: &str) -> IResult<&str, AstNode> {
    parse_primary_expr(input)
}
