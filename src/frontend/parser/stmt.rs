// src/frontend/parser/stmt.rs
use crate::frontend::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::opt;
use nom::multi::many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;

use super::parser::ws;
use super::expr::parse_full_expr;

pub fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    let (input, lhs) = ws(parse_full_expr).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, rhs) = ws(parse_full_expr).parse(input)?;
    Ok((input, AstNode::Assign(Box::new(lhs), Box::new(rhs))))
}

pub fn parse_return(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("return")).parse(input)?;
    let (input, inner) = ws(parse_full_expr).parse(input)?;
    Ok((input, AstNode::Return(Box::new(inner))))
}

pub fn parse_if(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("if")).parse(input)?;
    let (input, cond) = ws(parse_full_expr).parse(input)?;
    let (input, then) = delimited(
        ws(tag("{")),
        many0(ws(parse_stmt)),
        ws(tag("}"))
    ).parse(input)?;
    let (input, else_opt) = opt(preceded(
        ws(tag("else")),
        delimited(
            ws(tag("{")),
            many0(ws(parse_stmt)),
            ws(tag("}"))
        )
    )).parse(input)?;
    let else_ = else_opt.unwrap_or_default();
    Ok((input, AstNode::If { cond: Box::new(cond), then, else_ }))
}

pub fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_assign,
        parse_return,
        parse_if,
        parse_full_expr
    )).parse(input)
}
