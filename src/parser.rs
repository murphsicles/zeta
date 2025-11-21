// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1, i64},
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded},
    IResult,
};

fn identifier(input: &str) -> IResult<&str, String> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        String::from,
    )(input)
}

fn parse_lit(input: &str) -> IResult<&str, AstNode> {
    map(i64, AstNode::Lit)(input)
}

fn parse_var(input: &str) -> IResult<&str, AstNode> {
    map(identifier, AstNode::Var)(input)
}

fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_lit,
        parse_var,
        delimited(tag("("), parse_expr, tag(")")),
    ))(input)
}

fn parse_method_call(input: &str) -> IResult<&str, AstNode> {
    let (mut i, mut expr) = parse_primary(input)?;

    while let Ok((i2, _)) = delimited(multispace0, tag("."), multispace0)(i) {
        let (i3, method) = identifier(i2)?;
        let (i4, arg) = delimited(
            tag("("),
            delimited(multispace0, parse_expr, multispace0),
            tag(")"),
        )(i3)?;
        i = i4;
        expr = AstNode::Call {
            receiver: format!("{:?}", expr), // temporary – will be proper expr tree later
            method,
            args: vec![format!("{:?}", arg)],
        };
    }

    Ok((i, expr))
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    parse_method_call(input)
}

fn parse_let(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = preceded(multispace0, tag("let"))(input)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = identifier(i)?;
    let (i, _) = delimited(multispace0, tag("="), multispace0)(i)?;
    let (i, expr) = parse_expr(i)?;
    let (i, _) = delimited(multispace0, tag(";"), multispace0)(i)?;
    Ok((i, AstNode::Assign(name, Box::new(expr))))
}

fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_let,
        map(parse_expr, |e| {
            // last expr in block becomes return
            AstNode::Assign("_".to_string(), Box::new(e))
        }),
    ))(input)
}

fn parse_block(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(
        delimited(multispace0, tag("{"), multispace0),
        many0(preceded(multispace0, parse_stmt)),
        delimited(multispace0, tag("}"), multispace0),
    )(input)
}

fn parse_return_type(input: &str) -> IResult<&str, String> {
    preceded(
        delimited(multispace0, tag("->"), multispace1),
        identifier,
    )(input)
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = preceded(multispace0, tag("fn"))(input)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = identifier(i)?;
    let (i, _) = delimited(multispace0, tag("("), multispace0)(i)?;
    let (i, _) = delimited(multispace0, tag(")"), multispace0)(i)?;
    let (i, ret) = opt(parse_return_type)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, body) = parse_block(i)?;

    let ret_expr = body.iter().rev().find_map(|node| {
        if let AstNode::Assign(name, expr) = node {
            if name == "_" { Some(expr.clone()) } else { None }
        } else {
            None
        }
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
