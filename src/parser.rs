// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1},
    combinator::{map, opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
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

fn parse_let(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = preceded(multispace0, tag("let"))(input)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = identifier(i)?;
    let (i, _) = preceded(multispace0, tag("="))(i)?;
    let (i, _) = multispace1(i)?;
    let (i, expr) = parse_expr(i)?;
    let (i, _) = delimited(multispace0, tag(";"), multispace0)(i)?;
    Ok((i, AstNode::Assign(name, Box::new(expr))))
}

fn parse_lit(input: &str) -> IResult<&str, AstNode> {
    map(
        nom::character::complete::i64(input),
        AstNode::Lit,
    )(input)
}

fn parse_var(input: &str) -> IResult<&str, AstNode> {
    map(identifier, AstNode::Var)(input)
}

fn parse_method_call(input: &str) -> IResult<&str, AstNode> {
    let (i, receiver) = parse_primary(i)?;
    let (i, calls) = many0(preceded(
        delimited(multispace0, tag("."), multispace0),
        pair(identifier, delimited(tag("("), delimited(multispace0, parse_expr, multispace0), tag(")"))),
    ))(i)?;

    let mut current = receiver;
    for (method, arg) in calls {
        current = AstNode::Call {
            receiver: match &current {
                AstNode::Var(v) => v.clone(),
                _ => format!("{:?}", current), // will be fixed after proper expr tree
            },
            method,
            args: vec![match arg {
                AstNode::Lit(n) => n.to_string(),
                AstNode::Var(v) => v,
                _ => "".to_string(),
            }],
        };
    }
    Ok((i, current))
}

fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((parse_lit, parse_var, delimited(tag("("), parse_expr, tag(")"))))(input)
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    parse_method_call(input)
}

fn parse_block(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(
        delimited(multispace0, tag("{"), multispace0),
        many0(alt((parse_let, map(parse_expr, |e| {
            // allow bare expr as stmt if it ends with ;
            if input.ends_with(';') { AstNode::Assign("_".to_string(), Box::new(e)) } else { e }
        })))),
        delimited(multispace0, tag("}"), multispace0),
    )(input)
}

fn parse_return_type(input: &str) -> IResult<&str, String> {
    preceded(
        delimited(multispace0, tag("->"), multispace1),
        map(identifier, String::from),
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

    Ok((
        i,
        AstNode::FuncDef {
            name,
            generics: vec![],
            params: vec![],
            ret: ret.unwrap_or("i32".to_string()),
            body,
            where_clause: None,
            attrs: vec![],
            ret_expr: body.last().cloned().map(Box::new),
        },
    ))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(preceded(multispace0, parse_func))(input)
}
