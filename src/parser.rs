// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1, i64},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, preceded, pair},
    IResult,
};

type PResult<'a, O> = IResult<&'a str, O>;

fn ident(input: &str) -> PResult<String> {
    let first = alt((alpha1, tag("_")));
    let rest = many0(alt((alphanumeric1, tag("_"))));
    map(pair(first, rest), |(f, r): (&str, Vec<&str>)| {
        let mut s = f.to_string();
        for p in r {
            s.push_str(p);
        }
        s
    })(input)
}

fn lit(input: &str) -> PResult<AstNode> {
    map(i64, AstNode::Lit)(input)
}

fn var(input: &str) -> PResult<AstNode> {
    map(ident, AstNode::Var)(input)
}

fn primary(input: &str) -> PResult<AstNode> {
    alt((
        lit,
        var,
        delimited(tag("("), expr, tag(")")),
    ))(input)
}

fn method_call(input: &str) -> PResult<AstNode> {
    let (mut rest, mut current) = primary(input)?;

    while let Ok((i2, _)) = delimited(multispace0, tag("."), multispace0)(rest) {
        let (i3, method) = ident(i2)?;
        let (i4, arg) = delimited(
            tag("("),
            delimited(multispace0, expr, multispace0),
            tag(")"),
        )(i3)?;
        current = AstNode::Call {
            receiver: Box::new(current),
            method,
            args: vec![arg],
            type_args: vec![],
        };
        rest = i4;
    }

    Ok((rest, current))
}

fn expr(input: &str) -> PResult<AstNode> {
    method_call(input)
}

fn let_stmt(input: &str) -> PResult<AstNode> {
    let (i, _) = preceded(multispace0, tag("let"))(input)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = delimited(multispace0, tag("="), multispace0)(i)?;
    let (i, e) = expr(i)?;
    let (i, _) = delimited(multispace0, tag(";"), multispace0)(i)?;
    Ok((i, AstNode::Assign(name, Box::new(e))))
}

fn stmt(input: &str) -> PResult<AstNode> {
    alt((
        let_stmt,
        map(expr, |e| AstNode::Assign("_".to_string(), Box::new(e))),
    ))(input)
}

fn block(input: &str) -> PResult<Vec<AstNode>> {
    delimited(
        delimited(multispace0, tag("{"), multispace0),
        many0(preceded(multispace0, stmt)),
        delimited(multispace0, tag("}"), multispace0),
    )(input)
}

fn ret_ty(input: &str) -> PResult<String> {
    preceded(delimited(multispace0, tag("->"), multispace1), ident)(input)
}

pub fn parse_func(input: &str) -> PResult<AstNode> {
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

pub fn parse_zeta(input: &str) -> PResult<Vec<AstNode>> {
    delimited(multispace0, many0(parse_func), multispace0)(input)
}
