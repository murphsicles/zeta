// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1, i64 as nom_i64},
    combinator::{map, opt},
    multi::many0,
    sequence::{delimited, preceded, tuple},
    IResult,
};

type Input<'a> = &'a str;
type Res<'a, O> = IResult<Input<'a>, O>;

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn ident(input: Input) -> Res<String> {
    map(
        tuple((
            nom::bytes::complete::take_while1(is_ident_start),
            nom::bytes::complete::take_while(is_ident_continue),
        )),
        |(head, tail): (Input, Input)| format!("{}{}", head, tail),
    )(input)
}

fn lit(input: Input) -> Res<AstNode> {
    map(nom_i64, AstNode::Lit)(input)
}

fn var(input: Input) -> Res<AstNode> {
    map(ident, AstNode::Var)(input)
}

fn expr(input: Input) -> Res<AstNode> {
    let parens = delimited(
        tag("("),
        delimited(multispace0, expr, multispace0),
        tag(")"),
    );

    let primary = alt((lit, var, parens));

    let method_call = move |input: Input| -> Res<AstNode> {
        let (mut rest, mut current) = primary(input)?;

        while let Ok((next, _)) = delimited(multispace0, tag("."), multispace0)(rest) {
            let (next, method) = delimited(multispace0, ident, multispace0)(next)?;
            let (next, arg) = delimited(
                tag("("),
                delimited(multispace0, expr, multispace0),
                tag(")"),
            )(next)?;
            current = AstNode::Call {
                receiver: Box::new(current),
                method,
                args: vec![arg],
                type_args: vec![],
            };
            rest = next;
        }

        Ok((rest, current))
    };

    method_call(input)
}

fn let_stmt(input: Input) -> Res<AstNode> {
    let (i, _) = preceded(multispace0, tag("let"))(input)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = delimited(multispace0, tag("="), multispace0)(i)?;
    let (i, e) = expr(i)?;
    let (i, _) = delimited(multispace0, tag(";"), multispace0)(i)?;
    Ok((i, AstNode::Assign(name, Box::new(e))))
}

fn stmt(input: Input) -> Res<AstNode> {
    alt((
        let_stmt,
        map(expr, |e| AstNode::Assign("_".to_string(), Box::new(e))),
    ))(input)
}

fn block(input: Input) -> Res<Vec<AstNode>> {
    delimited(
        delimited(multispace0, tag("{"), multispace0),
        many0(preceded(multispace0, stmt)),
        delimited(multispace0, tag("}"), multispace0),
    )(input)
}

fn ret_ty(input: Input) -> Res<String> {
    preceded(
        delimited(multispace0, tag("->"), multispace1),
        ident,
    )(input)
}

pub fn parse_func(input: Input) -> Res<AstNode> {
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

pub fn parse_zeta(input: Input) -> Res<Vec<AstNode>> {
    delimited(multispace0, many0(parse_func), multispace0)(input)
}
