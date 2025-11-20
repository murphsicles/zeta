use crate::ast::AstNode;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
};

fn identifier(input: &str) -> IResult<&str, String> {
    let (i, s) = nom::bytes::complete::is_a("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")(input)?;
    let (i, s2) = nom::bytes::complete::is_a("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")(i)?;
    Ok((i, format!("{}{}", s, s2)))
}

fn int_literal(input: &str) -> IResult<&str, AstNode> {
    map(digit1, |s: &str| AstNode::Lit(s.parse().unwrap()))(input)
}

fn parse_type(input: &str) -> IResult<&str, String> {
    identifier(input)
}

fn primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        int_literal,
        map(identifier, AstNode::Var),
        delimited(tag("("), expr, tag(")")),
    ))(input)
}

fn call_expr(input: &str) -> IResult<&str, AstNode> {
    let (i, recv) = primary(input)?;
    let (i, chain) = many0(tuple((
        preceded(tag("."), identifier),
        opt(delimited(tag("("), separated_list0(tag(","), expr), tag(")"))),
    )))(i)?;

    let mut cur = recv;
    for (method, args_opt) in chain {
        let args = args_opt.unwrap_or(vec![]);
        cur = AstNode::Call {
            receiver: Box::new(cur),
            method,
            args,
        };
    }
    Ok((i, cur))
}

fn expr(input: &str) -> IResult<&str, AstNode> {
    call_expr(input)
}

fn let_stmt(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("let")(input)?;
    let (i, _) = multispace0(i)?;
    let (i, name) = identifier(i)?;
    let (i, ty_opt) = opt(preceded(tuple((tag(":"), multispace0)), parse_type))(i)?;
    let (i, _) = tuple((multispace0, tag("="), multispace0))(i)?;
    let (i, rhs) = expr(i)?;
    Ok((i, AstNode::Let { name, ty: ty_opt, rhs: Box::new(rhs) }))
}

fn stmt(input: &str) -> IResult<&str, AstNode> {
    let (i, node) = alt((
        let_stmt,
        map(expr, |e| AstNode::ExprStmt(Box::new(e))),
    ))(input)?;
    let (i, _) = opt(tuple((multispace0, tag(";"))))(i)?;
    Ok((i, node))
}

fn body(input: &str) -> IResult<&str, (Vec<AstNode>, Option<Box<AstNode>>)> {
    let (i, stmts) = many0(preceded(multispace0, stmt))(input)?;
    let (i, ret) = opt(preceded(multispace0, expr))(i)?;
    Ok((i, (stmts, ret.map(Box::new))))
}

fn param(input: &str) -> IResult<&str, (String, String)> {
    let (i, name) = identifier(input)?;
    let (i, _) = tuple((multispace0, tag(":"), multispace0))(i)?;
    let (i, ty) = parse_type(i)?;
    Ok((i, (name, ty)))
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("fn")(input)?;
    let (i, _) = multispace0(i)?;
    let (i, name) = identifier(i)?;
    let (i, params) = delimited(
        tag("("),
        separated_list0(tuple((multispace0, tag(","), multispace0)), param),
        tag(")"),
    )(i)?;
    let (i, ret_opt) = opt(preceded(tuple((multispace0, tag("->"), multispace0)), parse_type))(i)?;
    let (i, _) = tuple((multispace0, tag("{"), multispace0))(i)?;
    let (i, (stmts, ret_expr)) = body(i)?;
    let (i, _) = tuple((multispace0, tag("}")))(i)?;

    Ok((i, AstNode::FuncDef {
        name,
        generics: vec![],
        params,
        ret: ret_opt.unwrap_or("i32".to_string()),
        body: stmts,
        ret_expr,
        where_clause: None,
        attrs: vec![],
    }))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(preceded(multispace0, parse_func))(input)
}
