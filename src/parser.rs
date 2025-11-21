// src/parser.rs
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as nom_i64, satisfy};
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use nom::Parser;

fn ws<'a, O>(inner: impl Parser<&'a str, O, nom::error::Error<&'a str>>) -> impl Parser<&'a str, O, nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

fn ident(input: &str) -> IResult<&str, String> {
    let (input, f) = satisfy(|c| c.is_alphabetic() || c == '_').parse(input)?;
    let (input, r) = many0(satisfy(|c| c.is_alphanumeric() || c == '_')).parse(input)?;
    let mut s = f.to_string();
    for c in r {
        s.push(c);
    }
    Ok((input, s))
}

fn literal(input: &str) -> IResult<&str, AstNode> {
    nom_i64.map(AstNode::Lit).parse(input)
}

fn variable(input: &str) -> IResult<&str, AstNode> {
    ident.map(AstNode::Var).parse(input)
}

fn parens(input: &str) -> IResult<&str, AstNode> {
    delimited(ws(tag("(")), expr, ws(tag(")"))).parse(input)
}

fn primary(input: &str) -> IResult<&str, AstNode> {
    alt((literal, variable, parens)).parse(input)
}

fn method_call(input: &str) -> IResult<&str, AstNode> {
    let (mut i, mut cur) = primary(input)?;
    loop {
        match ws(tag(".")).parse(i) {
            Ok((i2, _)) => {
                let (i3, method) = ident.parse(i2)?;
                let (i4, args) = delimited(ws(tag("(")), separated_list0(ws(tag(",")), expr), ws(tag(")"))).parse(i3)?;
                cur = AstNode::Call {
                    receiver: Box::new(cur),
                    method,
                    args,
                    type_args: vec![],
                };
                i = i4;
            }
            Err(_) => break,
        }
    }
    Ok((i, cur))
}

fn factor(input: &str) -> IResult<&str, AstNode> {
    let (mut i, mut left) = method_call(input)?;
    loop {
        match alt((ws(tag("*")), ws(tag("/")), ws(tag("%")))).parse(i) {
            Ok((i2, op)) => {
                let (i3, right) = method_call(i2)?;
                left = AstNode::Call {
                    receiver: Box::new(left),
                    method: match op {
                        "*" => "mul",
                        "/" => "div",
                        "%" => "rem",
                        _ => unreachable!(),
                    }.to_string(),
                    args: vec![right],
                    type_args: vec![],
                };
                i = i3;
            }
            Err(_) => break,
        }
    }
    Ok((i, left))
}

fn term(input: &str) -> IResult<&str, AstNode> {
    let (mut i, mut left) = factor(input)?;
    loop {
        match alt((ws(tag("+")), ws(tag("-")))).parse(i) {
            Ok((i2, op)) => {
                let (i3, right) = factor(i2)?;
                left = AstNode::Call {
                    receiver: Box::new(left),
                    method: if op == "+" { "add" } else { "sub" }.to_string(),
                    args: vec![right],
                    type_args: vec![],
                };
                i = i3;
            }
            Err(_) => break,
        }
    }
    Ok((i, left))
}

pub fn expr(input: &str) -> IResult<&str, AstNode> {
    term.parse(input)
}

fn let_stmt(input: &str) -> IResult<&str, AstNode> {
    preceded(
        ws(tag("let")),
        map(
            tuple((ident, ws(tag("=")), expr, ws(tag(";"))),
            |(name, _, e, _)| AstNode::Assign(name, Box::new(e)),
        ),
    ).parse(input)
}

fn expr_stmt(input: &str) -> IResult<&str, AstNode> {
    map(tuple((expr, ws(tag(";")))), |(e, _)| AstNode::Assign("_".to_string(), Box::new(e))).parse(input)
}

fn spawn_expr(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("spawn")).parse(input)?;
    let (i, actor_ty) = ident.parse(i)?;
    let (i, args) = delimited(ws(tag("(")), separated_list0(ws(tag(",")), expr), ws(tag(")"))).parse(i)?;
    Ok((i, AstNode::SpawnActor {
        actor_ty,
        init_args: args.into_iter().map(|_| "tmp".to_string()).collect::<Vec<String>>(),
    }))
}

fn await_expr(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("await")).parse(input)?;
    let (i, future) = expr.parse(i)?;
    Ok((i, AstNode::Await { expr: Box::new(future) }))
}

fn stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        let_stmt,
        expr_stmt,
        spawn_expr.map(|e| AstNode::Assign("_".to_string(), Box::new(e))),
        await_expr.map(|e| AstNode::Assign("_".to_string(), Box::new(e))),
    )).parse(input)
}

fn block(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(ws(tag("{")), many0(stmt), ws(tag("}"))).parse(input)
}

fn actor_field(input: &str) -> IResult<&str, (String, String)> {
    map(tuple((ident, ws(tag(":")), ident)), |(name, _, ty)| (name, ty)).parse(input)
}

fn parse_actor(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("actor")).parse(input)?;
    let (i, name) = ident.parse(i)?;
    let (i, state) = delimited(ws(tag("{"))), many0(preceded(multispace0, actor_field)), ws(tag("}"))).parse(i)?;
    Ok((i, AstNode::ActorDef {
        name,
        state,
        methods: vec![],
    }))
}

fn async_fn(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("async")).parse(input)?;
    let (i, _) = ws(tag("fn")).parse(i)?;
    let (i, name) = ident.parse(i)?;
    let (i, _) = ws(tag("(")).parse(i)?;
    let (i, _) = ws(tag(")")).parse(i)?;
    let (i, body) = block.parse(i)?;
    Ok((i, AstNode::AsyncFn {
        name,
        params: vec![],
        ret: "Future<i32>".to_string(),
        body,
    }))
}

fn ret_ty(input: &str) -> IResult<&str, String> {
    preceded(ws(tag("->")), ws(ident)).parse(input)
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = preceded(ws(tag("fn")), multispace1).parse(input)?;
    let (i, name) = ident.parse(i)?;
    let (i, _) = ws(tag("(")).parse(i)?;
    let (i, _) = ws(tag(")")).parse(i)?;
    let (i, ret) = opt(ret_ty).parse(i)?;
    let (i, body) = block.parse(i)?;

    let ret_expr = body.iter().rev().find_map(|n| match n {
        AstNode::Assign(s, e) if s == "_" => Some(e.clone()),
        _ => None,
    });

    Ok((i, AstNode::FuncDef {
        name,
        generics: vec![],
        params: vec![],
        ret: ret.unwrap_or_else(|| "i32".to_string()),
        body,
        where_clause: None,
        attrs: vec![],
        ret_expr,
    }))
}

fn parse_top(input: &str) -> IResult<&str, AstNode> {
    alt((parse_actor, async_fn, parse_func)).parse(input)
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(parse_top), multispace0).parse(input)
}
