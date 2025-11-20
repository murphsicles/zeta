use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0, digit1, space0, space1},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
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
    let (i, name) = identifier(input)?;
    let (i, generics) = opt(delimited(
        tag("<"),
        separated_list1(preceded(space0, tag(",")), preceded(space0, parse_type)),
        preceded(space0, tag(">")),
    ))(i)?;
    Ok((i, if let Some(g) = generics {
        format!("{}<{}>", name, g.join(", "))
    } else {
        name
    }))
}

fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        int_literal,
        map(identifier, AstNode::Var),
        delimited(preceded(space0, tag("(")), preceded(space0, parse_expr), preceded(space0, tag(")"))),
    ))(input)
}

fn parse_method_call(input: &str) -> IResult<&str, AstNode> {
    let (i, mut expr) = parse_primary(input)?;
    let (mut i, calls) = many0(tuple((
        delimited(space0, tag("."), space0),
        identifier,
        opt(delimited(
            preceded(space0, tag("(")),
            separated_list0(preceded(space0, tag(",")), preceded(space0, parse_expr)),
            preceded(space0, tag(")")),
        )),
    )))(i)?;

    for (_, method, args_opt) in calls.drain(..) {
        let args = args_opt.unwrap_or_default();
        expr = AstNode::Call {
            receiver: Box::new(expr),
            method,
            args,
        };
    }
    Ok((i, expr))
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    parse_method_call(input)
}

fn parse_let_stmt(input: &str) -> IResult<&str, AstNode> {
    let (i, (_, _, name, ty_opt, _, _, rhs, _)) = tuple((
        tag("let"),
        space1,
        identifier,
        opt(preceded(delimited(space0, tag(":"), space0), parse_type)),
        delimited(space0, tag("="), space0),
        parse_expr,
        opt(preceded(space0, tag(";"))),
    ))(input)?;
    Ok((i, AstNode::Let {
        name,
        ty: ty_opt,
        rhs: Box::new(rhs),
    }))
}

fn parse_expr_stmt(input: &str) -> IResult<&str, AstNode> {
    let (i, expr) = parse_expr(input)?;
    let (i, _) = opt(tag(";"))(i)?;
    Ok((i, AstNode::ExprStmt(Box::new(expr))))
}

fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((parse_let_stmt, parse_expr_stmt))(input)
}

fn parse_body(input: &str) -> IResult<&str, (Vec<AstNode>, Option<Box<AstNode>>)> {
    let (i, stmts) = many0(delimited(space0, parse_stmt, space0))(input)?;
    let (i, ret_expr) = opt(preceded(space0, parse_expr))(i)?;
    Ok((i, (stmts, ret_expr.map(Box::new))))
}

fn parse_param(input: &str) -> IResult<&str, (String, String)> {
    separated_pair(
        identifier,
        delimited(space0, tag(":"), space0),
        parse_type,
    )(input)
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, (_, _, name, _, params, _, ret_ty_opt, _, _, body, _)) = tuple((
        delimited(space0, tag("fn"), space1),
        identifier,
        delimited(
            preceded(space0, tag("(")),
            separated_list0(preceded(space0, tag(",")), preceded(space0, parse_param)),
            preceded(space0, tag(")")),
        ),
        opt(preceded(delimited(space0, tag("->"), space0), identifier)),
        delimited(space0, tag("{"), space0),
        parse_body,
        delimited(space0, tag("}"), space0),
    ))(input)?;

    let (body_stmts, ret_expr) = body;
    let ret_ty = ret_ty_opt.unwrap_or("i32".to_string());

    Ok((i, AstNode::FuncDef {
        name,
        generics: vec![],
        params,
        ret: ret_ty,
        body: body_stmts,
        ret_expr,
        where_clause: None,
        attrs: vec![],
    }))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(delimited(multispace0, parse_func, multispace0))(input)
}
