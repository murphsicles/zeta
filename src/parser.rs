use crate::ast::AstNode;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
};

fn identifier(input: &str) -> IResult<&str, String> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
    .map(|(i, s)| (i, s.to_string()))
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
    let (i, chain) = many0(pair(
        preceded(tag("."), identifier),
        opt(delimited(tag("("), separated_list0(tag(","), expr), tag(")"))),
    ))(i)?;

    let mut cur = recv;
    for (method, args) in chain {
        cur = AstNode::Call {
            receiver: Box::new(cur),
            method,
            args: args.unwrap_or_default(),
        };
    }
    Ok((i, cur))
}

fn expr(input: &str) -> IResult<&str, AstNode> {
    call_expr(input)
}

fn let_stmt(input: &str) -> IResult<&str, AstNode> {
    let (i, (_, _, name, ty_opt, _, rhs)) = tuple((
        tag("let"),
        multispace1,
        identifier,
        opt(preceded(preceded(tag(":"), multispace0), parse_type)),
        preceded(multispace0, tag("=")),
        preceded(multispace0, expr),
    ))(input)?;
    Ok((i, AstNode::Let { name, ty: ty_opt, rhs: Box::new(rhs) }))
}

fn stmt(input: &str) -> IResult<&str, AstNode> {
    let (i, node) = alt((let_stmt, map(expr, |e| AstNode::ExprStmt(Box::new(e)))))(input)?;
    let (i, _) = opt(tag(";"))(i)?;
    Ok((i, node))
}

fn body(input: &str) -> IResult<&str, (Vec<AstNode>, Option<Box<AstNode>>)> {
    let (i, stmts) = many0(stmt)(input)?;
    let (i, ret) = opt(expr)(i)?;
    Ok((i, (stmts, ret.map(Box::new))))
}

fn param(input: &str) -> IResult<&str, (String, String)> {
    separated_pair(
        identifier,
        preceded(multispace0, tag(":")),
        preceded(multispace0, parse_type),
    )(input)
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, (_, name, _, params, _, ret_opt, _, (stmts, ret_expr))) = tuple((
        tag("fn"),
        preceded(multispace1, identifier),
        delimited(tag("("), separated_list0(tag(","), preceded(multispace0, param)), tag(")")),
        opt(preceded(preceded(tag("->"), multispace0), parse_type)),
        preceded(multispace0, tag("{")),
        body,
        tag("}"),
    ))(input)?;

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
