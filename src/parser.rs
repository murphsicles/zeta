use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0, digit1},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

fn identifier(input: &str) -> IResult<&str, String> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input).map(|(i, s)| (i, s.to_string()))
}

fn int_literal(input: &str) -> IResult<&str, AstNode> {
    map(digit1, |s: &str| AstNode::Lit(s.parse().unwrap()))(input)
}

fn parse_type(input: &str) -> IResult<&str, String> {
    let (i, name) = identifier(input)?;
    let (i, generics) = opt(delimited(
        tag("<"),
        separated_list1(tag(","), parse_type),
        tag(">"),
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
        delimited(tag("("), parse_expr, tag(")")),
    ))(input)
}

fn parse_method_call(input: &str) -> IResult<&str, AstNode> {
    let (i, receiver) = parse_primary(input)?;
    let (i, calls) = many0(tuple((
        preceded(tag("."), identifier),
        opt(delimited(tag("("), separated_list0(tag(","), parse_expr), tag(")"))),
    )))(i)?;

    let mut current = receiver;
    for (method, args_opt) in calls {
        let args = args_opt.unwrap_or_default();
        current = AstNode::Call {
            receiver: Box::new(current),
            method,
            args,
        };
    }
    Ok((i, current))
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    parse_method_call(input)
}

fn parse_let(input: &str) -> IResult<&str, AstNode> {
    let (i, (_, name, ty_opt, _, rhs)) = tuple((
        tag("let"),
        preceded(multispace0, identifier),
        opt(preceded(preceded(multispace0, tag(":")), preceded(multispace0, parse_type))),
        preceded(multispace0, tag("=")),
        preceded(multispace0, parse_expr),
    ))(input)?;
    Ok((i, AstNode::Let { name, ty: ty_opt, rhs: Box::new(rhs) }))
}

fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_let,
        map(parse_expr, |e| AstNode::ExprStmt(Box::new(e))),
    ))(input)
}

fn parse_body(input: &str) -> IResult<&str, (Vec<AstNode>, Option<Box<AstNode>>)> {
    let (i, stmts) = many0(terminated(parse_stmt, tag(";")))(input)?;
    let (i, ret) = opt(parse_expr)(i)?;
    Ok((i, (stmts, ret.map(Box::new))))
}

fn parse_param(input: &str) -> IResult<&str, (String, String)> {
    separated_pair(
        identifier,
        preceded(multispace0, tag(":")),
        preceded(multispace0, parse_type),
    )(input)
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, (_, name, _, params, _, ret_ty, _, body)) = tuple((
        tag("fn"),
        preceded(multispace0, identifier),
        delimited(tag("("), separated_list0(tag(","), preceded(multispace0, parse_param)), tag(")")),
        opt(preceded(preceded(multispace0, tag("->")), preceded(multispace0, identifier))),
        preceded(multispace0, tag("{")),
        delimited(multispace0, parse_body, preceded(multispace0, tag("}"))),
    ))(input)?;

    let (body_stmts, ret_expr) = body;
    let ret_ty = ret_ty.unwrap_or("()".to_string());

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
    many0(preceded(multispace0, parse_func))(input)
}
