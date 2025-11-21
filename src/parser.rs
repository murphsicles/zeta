// src/parser.rs
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{i64 as nom_i64, multispace0, satisfy};
use nom::combinator::map;
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use nom::Parser;

fn ws<'a, O>(inner: impl Parser<&'a str, O, nom::error::Error<&'a str>> + Copy) -> impl Parser<&'a str, O, nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

fn ident(input: &str) -> IResult<&str, String> {
    let (input, first) = satisfy(|c| c.is_alphabetic() || c == '_')(input)?;
    let (input, rest) = many0(satisfy(|c| c.is_alphanumeric() c == '_'))(input)?;
    Ok((input, std::iter::once(first).chain(rest).collect()))
}

fn literal(input: &str) -> IResult<&str, AstNode> {
    map(nom_i64, AstNode::Lit).parse(input)
}

fn variable(input: &str) -> IResult<&str, AstNode> {
    map(ident, AstNode::Var).parse(input)
}

fn parens(input: &str) -> IResult<&str, AstNode> {
    delimited(ws(tag("(")), expr, ws(tag(")"))).parse(input)
}

fn fn_call(input: &str) -> IResult<&str, AstNode> {
    let (input, name) = ident(input)?;
    let (input, args) = delimited(ws(tag("(")), separated_list0(ws(tag(",")), expr), ws(tag(")")))(input)?;
    Ok((input, AstNode::Call {
        receiver: None,
        method: name,
        args,
        type_args: vec![],
    }))
}

fn primary(input: &str) -> IResult<&str, AstNode> {
    alt((fn_call, literal, variable, parens)).parse(input)
}

fn method_call(input: &str) -> IResult<&str, AstNode> {
    let (mut i, mut cur) = primary(input)?;
    loop {
        if let Ok((i2, _)) = ws(tag("."))(i) {
            let (i3, method) = ident(i2)?;
            let (i4, args) = delimited(ws(tag("(")), separated_list0(ws(tag(",")), expr), ws(tag(")")))(i3)?;
            cur = AstNode::Call {
                receiver: Some(Box::new(cur)),
                method,
                args,
                type_args: vec![],
            };
            i = i4;
        } else {
            break;
        }
    }
    Ok((i, cur))
}

fn factor(input: &str) -> IResult<&str, AstNode> {
    let (mut i, mut left) = method_call(input)?;
    while let Ok((i2, op)) = alt((ws(tag("*")), ws(tag("/")), ws(tag("%"))))(i) {
        let (i3, right) = method_call(i2)?;
        left = AstNode::Call {
            receiver: Some(Box::new(left)),
            method: match op {
                "*" => "mul".to_string(),
                "/" => "div".to_string(),
                "%" => "rem".to_string(),
                _ => unreachable!(),
            },
            args: vec![right],
            type_args: vec![],
        };
        i = i3;
    }
    Ok((i, left))
}

fn term(input: &str) -> IResult<&str, AstNode> {
    let (mut i, mut left) = factor(input)?;
    while let Ok((i2, op)) = alt((ws(tag("+")), ws(tag("-"))))(i) {
        let (i3, right) = factor(i2)?;
        left = AstNode::Call {
            receiver: Some(Box::new(left)),
            method: if op == "+" { "add" } else { "sub" }.to_string(),
            args: vec![right],
            type_args: vec![],
        };
        i = i3;
    }
    Ok((i, left))
}

pub fn expr(input: &str) -> IResult<&str, AstNode> {
    term(input)
}

fn let_stmt(input: &str) -> IResult<&str, AstNode> {
    preceded(
        ws(tag("let")),
        map(
            tuple((ident, ws(tag("=")), expr, ws(tag(";"))),
            |(name, _, e, _)| AstNode::Assign(name, Box::new(e)),
        ),
    )
    .parse(input)
}

fn expr_stmt(input: &str) -> IResult<&str, AstNode> {
    map(tuple((expr, ws(tag(";")))), |(e, _)| {
        AstNode::Assign("_".to_string(), Box::new(e))
    })
    .parse(input)
}

fn spawn_expr(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("spawn"))(input)?;
    let (i, actor_ty) = ident(i)?;
    let (i, args) = delimited(ws(tag("(")), separated_list0(ws(tag(",")), expr), ws(tag(")"))(i)?;
    Ok((
        i,
        AstNode::SpawnActor {
            actor_ty,
            init_args: args.into_iter().map(|_| "tmp".to_string()).collect(),
        },
    ))
}

fn await_expr(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("await"))(input)?;
    let (i, future) = expr(i)?;
    Ok((i, AstNode::Await { expr: Box::new(future) }))
}

fn stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        let_stmt,
        expr_stmt,
        map(spawn_expr, |e| AstNode::Assign("_".to_string(), Box::new(e))),
        map(await_expr, |e| AstNode::Assign("_".to_string(), Box::new(e))),
    ))
    .parse(input)
}

fn block(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(ws(tag("{")), many0(stmt), ws(tag("}"))).parse(input)
}

fn actor_field(input: &str) -> IResult<&str, (String, String)> {
    map(tuple((ident, ws(tag(":")), ident)), |(n, _, t)| (n, t)).parse(input)
}

fn parse_actor(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("actor"))(input)?;
    let (i, name) = ident(i)?;
    let (i, state) = delimited(ws(tag("{")), many0(preceded(multispace0, actor_field)), ws(tag("}")))(i)?;
    Ok((
        i,
        AstNode::ActorDef {
            name,
            state,
            methods: vec![],
        },
    ))
}

fn async_fn(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = ws(tag("async"))(input)?;
    let (i, _) = ws(tag("fn"))(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = ws(tag("("))(i)?;
    let (i, _) = ws(tag(")"))(i)?;
    let (i, body) = block(i)?;
    Ok((
        i,
        AstNode::AsyncFn {
            name,
            params: vec![],
            ret: "Future<i64>".to_string(),
            body,
        },
    ))
}

fn ret_ty(input: &str) -> IResult<&str, String> {
    preceded(ws(tag("->")), ws(ident)).parse(input)
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = preceded(ws(tag("fn")), multispace0)(input)?; // allow no space after fn
    let (i, name) = ident(i)?;
    let (i, _) = ws(tag("("))(i)?;
    let (i, _) = ws(tag(")"))(i)?;
    let (i, ret) = opt(ret_ty)(i)?;
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
            ret: ret.unwrap_or_else(|| "i64".to_string()),
            body,
            where_clause: None,
            attrs: vec![],
            ret_expr,
        },
    ))
}

fn parse_top(input: &str) -> IResult<&str, AstNode> {
    alt((parse_actor, async_fn, parse_func)).parse(input)
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    delimited(multispace0, many0(parse_top), multispace0).parse(input)
}
