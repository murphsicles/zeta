// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1, i64},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, preceded},
    IResult,
};

fn ident(input: &str) -> IResult<&str, String> {
    let first = alt((alpha1, tag("_")));
    let rest = many0(alt((alphanumeric1, tag("_"))));
    map(nom::sequence::pair(first, rest), |(f, r): (&str, Vec<&str>)| {
        let mut s = f.to_string();
        for part in r {
            s.push_str(part);
        }
        s
    })(input)
}

fn lit(input: &str) -> IResult<&str, AstNode> {
    map(i64, AstNode::Lit)(input)
}

fn var(input: &str) -> IResult<&str, AstNode> {
    map(ident, AstNode::Var)(input)
}

fn primary(input: &str) -> IResult<&str, AstNode> {
    alt((lit, var, delimited(tag("("), expr, tag(")"))))(input)
}

fn method_call(mut input: &str) -> IResult<&str, AstNode> {
    let (i, mut current) = primary(input)?;
    input = i;

    while let Ok((i2, _)) = delimited(multispace0, tag("."), multispace0)(input) {
        let (i3, method) = ident(i2)?;
        let (i4, arg) = delimited(
            tag("("),
            delimited(multispace0, expr, multispace0),
            tag(")"),
        )(i3)?;
        current = AstNode::Call {
            receiver: "".to_string(), // placeholder – will be proper later
            method,
            args: vec![format!("{:?}", arg)],
        };
        input = i4;
    }
    Ok((input, current))
}

fn expr(input: &str) -> IResult<&str, AstNode> {
    method_call(input)
}

fn let_stmt(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = preceded(multispace0, tag("let"))(input)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = delimited(multispace0, tag("="), multispace0)(i)?;
    let (i, e) = expr(i)?;
    let (i, _) = delimited(multispace0, tag(";"), multispace0)(i)?;
    Ok((i, AstNode::Assign(name, Box::new(e))))
}

fn stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        let_stmt,
        map(expr, |e| AstNode::Assign("_".to_string(), Box::new(e))),
    ))(input)
}

fn block(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(
        delimited(multispace0, tag("{"), multispace0),
        many0(preceded(multispace0, stmt)),
        delimited(multispace0, tag("}"), multispace0),
    )(input)
}

fn ret_ty(input: &str) -> IResult<&str, String> {
    preceded(delimited(multispace0, tag("->"), multispace1), ident)(input)
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = preceded(multispace0, tag("fn"))(input)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = delimited(multispace0, tag("("), multispace0)(i)?;
    let (i, _) = delimited(multispace0, tag(")"), multispace0)(i)?;
    let (i, ret) = opt(ret_ty)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, body) = block(i)?;

    let ret_expr = body.iter().rev().find_map(|n| match n {
        AstNode::Assign(s, e) if s == "_" => Some(e.clone()),
        _ => None,
    });

    Ok((
        i,
        AstNode::FuncDef {
            name,
            generics: vec![],
            params: vec![],
            ret: ret.unwrap_or_else(|| "i32".to_string()),
            body,
            where_clause: None,
            attrs: vec![],
            ret_expr,
        },
    ))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(parse_func), multispace0)(input)
}
