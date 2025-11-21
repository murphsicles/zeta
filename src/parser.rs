// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1, i64 as nom_i64},
    combinator::{map, opt, value},
    multi::many0,
    sequence::{delimited, preceded, tuple},
    IResult,
};

type Input<'a> = &'a str;
type Res<'a, O> = IResult<Input<'a>, O>;

fn is_ident_start(c: char) -> bool { c.is_alphabetic() || c == '_' }
fn is_ident_continue(c: char) -> bool { c.is_alphanumeric() || c == '_' }

fn ident(input: Input) -> Res<String> {
    let first = take_while1(is_ident_start);
    let rest = take_while1(is_ident_continue);
    map(tuple((first, many0(rest))), |(f, r)| {
        let mut s = f.to_owned();
        for part in r {
            s.push_str(part);
        }
        s
    })(input)
}

fn lit(input: Input) -> Res<AstNode> {
    map(nom_i64, AstNode::Lit)(input)
}

fn var(input: Input) -> Res<AstNode> {
    map(ident, AstNode::Var)(input)
}

fn parens(input: Input) -> Res<AstNode> {
    delimited(tag("("), delimited(multispace0, expr, multispace0), tag(")"))(input)
}

fn primary(input: Input) -> Res<AstNode> {
    alt((lit, var, parens))(input)
}

fn method_call(input: Input) -> Res<AstNode> {
    let (mut i, mut cur) = primary(input)?;

    while let Ok((i2, _)) = delimited(multispace0, tag("."), multispace0)(i) {
        let (i3, method) = delimited(multispace0, ident, multispace0)(i2)?;
        let (i4, arg) = delimited(
            tag("("),
            delimited(multispace0, expr, multispace0),
            tag(")"),
        )(i3)?;
        cur = AstNode::Call {
            receiver: Box::new(cur),
            method,
            args: vec![arg],
            type_args: vec![],
        };
        i = i4;
    }
    Ok((i, cur))
}

fn expr(input: Input) -> Res<AstNode> {
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
        AstNode::Assign(s, e) if s == "_" => Some(Box::new((*e).clone())),
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
