// src/parser.rs
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as nom_i64, multispace0, satisfy};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, preceded};
use nom::IResult;
use nom::Parser;

fn ws<'a, O>(inner: impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> + Copy) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

fn ident(input: &str) -> IResult<&str, String> {
    let (i, c) = satisfy(|c| c.is_alphabetic() || c == '_').parse(input)?;
    let (i, rest) = many0(satisfy(|c| c.is_alphanumeric() || c == '_')).parse(i)?;
    Ok((i, std::iter::once(c).chain(rest).collect()))
}

fn literal(input: &str) -> IResult<&str, AstNode> {
    map(nom_i64, |n| AstNode::Lit(n)).parse(input)
}

fn variable(input: &str) -> IResult<&str, AstNode> {
    map(ident, |s| AstNode::Var(s)).parse(input)
}

fn expr(input: &str) -> IResult<&str, AstNode> {
    let (i, left) = alt((literal, variable)).parse(input)?;
    let plus = ws(tag("+"));
    match plus.parse(i) {
        Ok((i, _)) => {
            let (i, right) = alt((literal, variable)).parse(i)?;
            Ok((i, AstNode::Call {
                receiver: Some(Box::new(left)),
                method: "add".to_string(),
                args: vec![right],
                type_args: vec![],
            }))
        }
        Err(nom::Err::Error(_)) => Ok((i, left)),
        Err(e) => Err(e),
    }
}

fn func_body(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(ws(tag("{")), many0(ws(expr)), ws(tag("}"))).parse(input)
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("fn")).parse(input)?;
    let (i, name) = ident(i)?;
    let (i, _) = ws(tag("(")).parse(i)?;
    let (i, _) = ws(tag(")")).parse(i)?;
    let (i, ret) = opt(preceded(ws(tag("->")), ws(ident))).parse(i)?;
    let (i, body) = func_body(i)?;
    Ok((i, AstNode::FuncDef {
        name,
        generics: vec![],
        params: vec![],
        ret: ret.map(|s| s.to_string()).unwrap_or_else(|| "i64".to_string()),
        body,
        attrs: vec![],
        ret_expr: None,
    }))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(ws(parse_func)), multispace0).parse(input)
}
