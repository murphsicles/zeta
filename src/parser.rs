// src/parser.rs
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as nom_i64, multispace0, satisfy};
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use nom::Parser;

fn ws<'a, O>(inner: impl Parser<&'a str, O, nom::error::Error<&'a str>> + Copy)
    -> impl Parser<&'a str, O, nom::error::Error<&'a str>>
{
    delimited(multispace0, inner, multispace0)
}

fn ident(input: &str) -> IResult<&str, String> {
    let (i, c) = satisfy(|c| c.is_alphabetic() || c == '_')(input)?;
    let (i, rest) = many0(satisfy(|c| c.is_alphanumeric() || c == '_'))(i)?;
    Ok((i, std::iter::once(c).chain(rest).collect()))
}

fn literal(input: &str) -> IResult<&str, AstNode> {
    map(nom_i64, AstNode::Lit).parse(input)
}

fn variable(input: &str) -> IResult<&str, AstNode> {
    map(ident, AstNode::Var).parse(input)
}

fn expr(input: &str) -> IResult<&str, AstNode> {
    let (i, left) = alt((literal, variable))(input)?;
    let (i, _) = ws(tag("+"))(i).ok();
    if let Ok((i, right)) = alt((literal, variable))(i) {
        return Ok((i, AstNode::Add(Box::new(left), Box::new(right))));
    }
    Ok((i, left))
}

fn func_body(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(ws(tag("{")), many0(expr), ws(tag("}")))(input)
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("fn"))(input)?;
    let (i, name) = ident(i)?;
    let (i, _) = ws(tag("("))(i)?;
    let (i, _) = ws(tag(")"))(i)?;
    let (i, ret) = opt(preceded(ws(tag("->")), ws(ident)))(i)?;
    let (i, body) = func_body(i)?;
    Ok((
        i,
        AstNode::FuncDef {
            name,
            generics: vec![],
            params: vec![],
            ret: ret.unwrap_or_else(|| "i64".to_string()),
            body,
            attrs: vec![],
            ret_expr: None,
        },
    ))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(parse_func), multispace0)(input)
}
