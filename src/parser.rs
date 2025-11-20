use crate::ast::AstNode;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
};

fn identifier(input: &str) -> IResult<&str, String> {
    recognize(tuple((
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    )))(input)
    .map(|(i, s)| (i, s.to_string()))
}

fn int_literal(input: &str) -> IResult<&str, AstNode> {
    map(digit1, |s: &str| AstNode::Lit(s.parse().unwrap()))(input)
}

fn var(input: &str) -> IResult<&str, AstNode> {
    map(identifier, AstNode::Var)(input)
}

fn primary_expr(input: &str) -> IResult<&str, AstNode> {
    alt((
        int_literal,
        var,
        delimited(tag("("), expr, tag(")")),
    ))(input)
}

fn call_expr(input: &str) -> IResult<&str, AstNode> {
    let (input, recv) = primary_expr(input)?;
    let (input, calls) = many0(tuple((
        preceded(tag("."), identifier),
        opt(delimited(tag("("), separated_list1(tag(","), expr), tag(")"))),
    )))(input)?;

    let mut current = recv;
    for (method, args) in calls {
        current = AstNode::Call {
            receiver: Box::new(current),
            method,
            args: args.unwrap_or_default(),
        };
    }
    Ok((input, current))
}

fn expr(input: &str) -> IResult<&str, AstNode> {
    call_expr(input)
}

fn let_stmt(input: &str) -> IResult<&str, AstNode> {
    let (input, (_, name, ty, _, rhs)) = tuple((
        tag("let"),
        preceded(multispace0, identifier),
        opt(preceded(preceded(tag(":"), multispace0), identifier)),
        preceded(multispace0, tag("=")),
        preceded(multispace0, expr),
    ))(input)?;
    Ok((input, AstNode::Let {
        name,
        ty,
        rhs: Box::new(rhs),
    }))
}

fn stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        let_stmt,
        map(expr, |e| AstNode::ExprStmt(Box::new(e))),
    ))(input)
}

fn body(input: &str) -> IResult<&str, (Vec<AstNode>, Option<Box<AstNode>>)> {
    let (input, stmts) = many0(terminated(stmt, tag(";")))(input)?;
    let (input, ret) = opt(expr)(input)?;
    Ok((input, (stmts, ret.map(Box::new))))
}

fn param(input: &str) -> IResult<&str, (String, String)> {
    separated_pair(identifier, preceded(multispace0, tag(":")), preceded(multispace0, identifier))(input)
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (input, (_, name, _, params, _, ret_ty, _, body_tuple)) = tuple((
        tag("fn"),
        preceded(multispace0, identifier),
        delimited(tag("("), separated_list1(tag(","), param), tag(")")),
        opt(preceded(preceded(tag("->"), multispace0), identifier)),
        preceded(multispace0, tag("{")),
        body,
        tag("}"),
    ))(input)?;

    let (body_stmts, ret_expr) = body_tuple;
    let ret = ret_ty.unwrap_or("i32".to_string());

    Ok((input, AstNode::FuncDef {
        name,
        generics: vec![],
        params,
        ret,
        body: body_stmts,
        ret_expr,
        where_clause: None,
        attrs: vec![],
    }))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(preceded(multispace0, parse_func))(input)
}
