// src/parser.rs
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as nom_i64, multispace0, satisfy};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::{delimited, delimited, preceded};
use nom::IResult;

fn ws<'a, O>(inner: impl FnMut(&'a str) -> IResult<&'a str, O>) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    delimited(multispace0, inner, multispace0)
}

fn ident(input: &str) -> IResult<&str, String> {
    let (input, c) = satisfy(|c| c.is_alphabetic() || c == '_')(input)?;
    let (input, rest) = many0(satisfy(|c| c.is_alphanumeric() || c == '_'))(input)?;
    Ok((input, std::iter::once(c).chain(rest).collect()))
}

fn literal(input: &str) -> IResult<&str, AstNode> {
    map(nom_i64, AstNode::Lit)(input)
}

fn variable(input: &str) -> IResult<&str, AstNode> {
    map(ident, AstNode::Var)(input)
}

fn expr(input: &str) -> IResult<&str, AstNode> {
    let (mut input, mut left) = alt((literal, variable))(input)?;

    loop {
        match ws(tag("+"))(input) {
            Ok((i, _)) => {
                input = i;
                let (i, right) = alt((literal, variable))(input)?;
                input = i;
                left = AstNode::Call {
                    receiver: Some(Box::new(left)),
                    method: "add".to_string(),
                    args: vec![right],
                    type_args: vec![],
                };
            }
            Err(nom::Err::Error(_)) => break,
            Err(e) => return Err(e),
        }
    }

    Ok((input, left))
}

fn func_body(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(ws(tag("{")), many0(ws(expr)), ws(tag("}")))(input)
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("fn"))(input)?;
    let (input, name) = ident(input)?;
    let (input, _) = ws(tag("("))(input)?;
    let (input, _) = ws(tag(")"))(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(ident)))(input)?;
    let (input, body) = func_body(input)?;

    let ret = ret_opt.map(|s| s.to_string()).unwrap_or("i64".to_string());

    Ok((input, AstNode::FuncDef {
        name,
        generics: vec![],
        params: vec![],
        ret,
        body,
        attrs: vec![],
        ret_expr: None,
    }))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(ws(parse_func)), multispace0)(input)
}
