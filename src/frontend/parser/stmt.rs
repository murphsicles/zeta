// src/frontend/parser/stmt.rs
//! Module for parsing statements and patterns in the Zeta language.

use super::expr::parse_full_expr;
use super::parser::{parse_ident, parse_path, parse_type, skip_ws_and_comments, ws};
use super::top_level::parse_type_alias;
use crate::frontend::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{opt, peek};
use nom::error::Error as NomError;
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded, terminated};

pub fn parse_pattern(input: &str) -> IResult<&str, AstNode> {
    alt((
        tag("_").map(|_| AstNode::Ignore),
        delimited(
            ws(tag("(")),
            terminated(
                separated_list0(ws(tag(",")), ws(parse_pattern)),
                opt(ws(tag(","))),
            ),
            ws(tag(")")),
        )
        .map(AstNode::Tuple),
        parse_path_pattern,
        parse_ident.map(AstNode::Var),
    ))
    .parse(input)
}

fn parse_path_pattern(input: &str) -> IResult<&str, AstNode> {
    let (input, path) = parse_path(input)?;
    let variant = path.join("::");

    let (input, _) = skip_ws_and_comments(input)?;

    if let Ok((_i, _)) = ws(tag("(")).parse(input) {
        let (input, pats) = delimited(
            ws(tag("(")),
            terminated(
                separated_list0(ws(tag(",")), ws(parse_pattern)),
                opt(ws(tag(","))),
            ),
            ws(tag(")")),
        )
        .parse(input)?;
        return Ok((
            input,
            AstNode::StructPattern {
                variant,
                fields: pats
                    .into_iter()
                    .enumerate()
                    .map(|(i, p)| (i.to_string(), p))
                    .collect(),
                rest: false,
            },
        ));
    }

    if let Ok((_i, _)) = ws(tag("{")).parse(input) {
        let (input, fields) = separated_list0(ws(tag(",")), ws(parse_field)).parse(input)?;
        let (input, _) = opt(ws(tag(","))).parse(input)?;
        let (input, has_rest) = opt(ws(tag(".."))).parse(input)?;
        let (input, _) = opt(ws(tag(","))).parse(input)?;
        let (input, _) = ws(tag("}")).parse(input)?;
        return Ok((
            input,
            AstNode::StructPattern {
                variant,
                fields,
                rest: has_rest.is_some(),
            },
        ));
    }

    Ok((input, AstNode::Var(variant)))
}

fn parse_field(input: &str) -> IResult<&str, (String, AstNode)> {
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, colon) = opt(ws(tag(":"))).parse(input)?;
    let (input, pat) = if colon.is_some() {
        ws(parse_pattern).parse(input)?
    } else {
        (input, AstNode::Var(name.clone()))
    };
    Ok((input, (name, pat)))
}

pub fn parse_block_body(input: &str) -> IResult<&str, Vec<AstNode>> {
    let mut body = vec![];
    let mut current = input;
    loop {
        let (next, _) = skip_ws_and_comments(current)?;
        if next.is_empty()
            || peek(tag::<&str, &str, NomError<&str>>("}"))
                .parse(next)
                .is_ok()
            || peek(tag::<&str, &str, NomError<&str>>(")"))
                .parse(next)
                .is_ok()
            || peek(tag::<&str, &str, NomError<&str>>(","))
                .parse(next)
                .is_ok()
        {
            current = next;
            break;
        }

        if let Ok((next_stmt, stmt)) = parse_stmt(next) {
            body.push(stmt);
            current = next_stmt;
            continue;
        }
        if let Ok((next_expr, expr)) = parse_full_expr(next) {
            body.push(AstNode::ExprStmt {
                expr: Box::new(expr),
            });
            current = next_expr;
            continue;
        }

        return Err(nom::Err::Error(NomError::new(
            next,
            nom::error::ErrorKind::Many0,
        )));
    }
    Ok((current, body))
}

fn parse_let(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("let")).parse(input)?;
    let (input, mut_) = opt(ws(tag("mut"))).parse(input)?;
    let (input, pattern) = ws(parse_pattern).parse(input)?;
    let (input, ty) = opt(preceded(ws(tag(":")), ws(parse_type))).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, expr) = ws(parse_full_expr).parse(input)?;
    let (input, _) = opt(ws(tag(";"))).parse(input)?;
    Ok((
        input,
        AstNode::Let {
            mut_: mut_.is_some(),
            pattern: Box::new(pattern),
            ty,
            expr: Box::new(expr),
        },
    ))
}

fn parse_for(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("for")).parse(input)?;
    let (input, pattern) = ws(parse_pattern).parse(input)?;
    let (input, _) = ws(tag("in")).parse(input)?;
    let (input, expr) = ws(parse_full_expr).parse(input)?;
    let (input, body) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    Ok((
        input,
        AstNode::For {
            pattern: Box::new(pattern),
            expr: Box::new(expr),
            body,
        },
    ))
}

fn parse_loop(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("loop")).parse(input)?;
    let (input, body) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    Ok((input, AstNode::Loop { body }))
}

fn parse_unsafe(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("unsafe")).parse(input)?;
    let (input, body) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    Ok((input, AstNode::Unsafe { body }))
}

fn parse_if_let(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("if")).parse(input)?;
    let (input, _) = ws(tag("let")).parse(input)?;
    let (input, pattern) = ws(parse_pattern).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, expr) = ws(parse_full_expr).parse(input)?;
    let (input, then) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    let (input, else_opt) = opt(preceded(
        ws(tag("else")),
        delimited(ws(tag("{")), parse_block_body, ws(tag("}"))),
    ))
    .parse(input)?;
    let else_: Vec<AstNode> = else_opt.unwrap_or(vec![]);
    Ok((
        input,
        AstNode::IfLet {
            pattern: Box::new(pattern),
            expr: Box::new(expr),
            then,
            else_,
        },
    ))
}

fn parse_if(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("if")).parse(input)?;
    let (input, cond) = ws(parse_full_expr).parse(input)?;
    let (input, then) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    let (input, else_opt) = opt(preceded(
        ws(tag("else")),
        delimited(ws(tag("{")), parse_block_body, ws(tag("}"))),
    ))
    .parse(input)?;
    let else_: Vec<AstNode> = else_opt.unwrap_or(vec![]);
    Ok((
        input,
        AstNode::If {
            cond: Box::new(cond),
            then,
            else_,
        },
    ))
}

fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    let (input, lhs) = ws(parse_full_expr).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, rhs) = ws(parse_full_expr).parse(input)?;
    let (input, _) = opt(ws(tag(";"))).parse(input)?;
    Ok((input, AstNode::Assign(Box::new(lhs), Box::new(rhs))))
}

fn parse_return(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("return")).parse(input)?;
    let (input, inner) = ws(parse_full_expr).parse(input)?;
    let (input, _) = opt(ws(tag(";"))).parse(input)?;
    Ok((input, AstNode::Return(Box::new(inner))))
}

fn parse_break(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("break")).parse(input)?;
    let (input, expr_opt) = opt(ws(parse_full_expr)).parse(input)?;
    let (input, _) = opt(ws(tag(";"))).parse(input)?;
    Ok((input, AstNode::Break(expr_opt.map(Box::new))))
}

fn parse_continue(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("continue")).parse(input)?;
    let (input, expr_opt) = opt(ws(parse_full_expr)).parse(input)?;
    let (input, _) = opt(ws(tag(";"))).parse(input)?;
    Ok((input, AstNode::Continue(expr_opt.map(Box::new))))
}

fn parse_expr_stmt(input: &str) -> IResult<&str, AstNode> {
    let (input, expr) = parse_full_expr(input)?;
    let (input, _) = opt(ws(tag(";"))).parse(input)?;
    Ok((
        input,
        AstNode::ExprStmt {
            expr: Box::new(expr),
        },
    ))
}

pub fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_return,
        parse_break,
        parse_continue,
        parse_if_let,
        parse_if,
        parse_for,
        parse_loop,
        parse_unsafe,
        parse_let,
        parse_assign,
        parse_type_alias,
        parse_expr_stmt,
    ))
    .parse(input)
}
