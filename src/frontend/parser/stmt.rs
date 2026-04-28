// src/frontend/parser/stmt.rs
//! Module for parsing statements in the Zeta language.

use super::expr::{parse_condition, parse_full_expr};
use super::parser::{parse_type, skip_ws_and_comments, ws};
use super::pattern::parse_pattern;
use super::top_level::{parse_type_alias, parse_const, parse_func};
use crate::frontend::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt, peek};
use nom::error::Error as NomError;
use nom::sequence::{delimited, preceded};

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
            let name = match &stmt { AstNode::FuncDef { name, .. } => Some(name.as_str()), AstNode::ConstDef { name, .. } => Some(name.as_str()), _ => None };
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

fn parse_while(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("while")).parse(input)?;
    let (input, cond) = ws(parse_condition).parse(input)?;
    let (input, body) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    Ok((
        input,
        AstNode::While {
            cond: Box::new(cond),
            body,
        },
    ))
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

    // Parse else clause: either `else { ... }` or `else if ...`
    let (input, else_opt) = opt(preceded(
        ws(tag("else")),
        alt((
            // else { ... } block
            map(
                delimited(ws(tag("{")), parse_block_body, ws(tag("}"))),
                |body| body,
            ),
            // else if ... (parse as another if statement)
            map(parse_if, |if_node| vec![if_node]),
        )),
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
    use super::expr::parse_unary;
    
    // First try to parse the left-hand side (which includes unary prefix and postfix)
    let (input, lhs) = ws(parse_unary).parse(input)?;
    
    // Try to parse compound assignment operators first, then simple assignment
    let (input, op) = alt((
        ws(tag("+=")),
        ws(tag("-=")),
        ws(tag("*=")),
        ws(tag("/=")),
        ws(tag("%=")),
        ws(tag("&=")),
        ws(tag("|=")),
        ws(tag("^=")),
        ws(tag("<<=")),
        ws(tag(">>=")),
        ws(tag("=")),
    )).parse(input)?;
    
    let (input, rhs) = ws(parse_full_expr).parse(input)?;
    let (input, _) = opt(ws(tag(";"))).parse(input)?;
    
    if op == "=" {
        Ok((input, AstNode::Assign(Box::new(lhs), Box::new(rhs))))
    } else {
        // Compound assignment: produce AssignOp with the base operator (without =)
        let bin_op = op.trim_end_matches('=').to_string();
        Ok((input, AstNode::AssignOp {
            op: bin_op,
            target: Box::new(lhs),
            value: Box::new(rhs),
        }))
    }
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
        parse_while,
        parse_unsafe,
        parse_let,
        parse_assign,
        parse_type_alias,
        parse_const,
        parse_func,
        parse_expr_stmt,
    ))
    .parse(input)
}
