// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1, i64 as nom_i64},
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

type Input<'a> = &'a str;
type Res<'a, O> = IResult<Input<'a>, O>;

fn ws<'a, F, O>(inner: F) -> impl FnMut(Input<'a>) -> Res<'a, O>
where
    F: FnMut(Input<'a>) -> Res<'a, O>,
{
    delimited(multispace0, inner, multispace0)
}

fn ident(input: Input) -> Res<String> {
    let first = nom::character::complete::satisfy(|c| c.is_alphabetic() || c == '_');
    let rest = nom::character::complete::satisfy(|c| c.is_alphanumeric() || c == '_');
    map(pair(first, many0(rest)), |(f, r)| {
        let mut s = f.to_string();
        for c in r { s.push(c); }
        s
    })(input)
}

// === Primitives ===
fn literal(input: Input) -> Res<AstNode> {
    map(nom_i64, AstNode::Lit)(input)
}

fn variable(input: Input) -> Res<AstNode> {
    map(ident, AstNode::Var)(input)
}

fn parens(input: Input) -> Res<AstNode> {
    delimited(ws(tag("(")), expr, ws(tag(")")))(input)
}

fn primary(input: Input) -> Res<AstNode> {
    alt((literal, variable, parens))(input)
}

// === Method calls ===
fn method_call(input: Input) -> Res<AstNode> {
    let (mut i, mut cur) = primary(input)?;

    while let Ok((i2, _)) = ws(tag("."))(i) {
        let (i3, method) = ident(i2)?;
        let (i4, args) = delimited(
            ws(tag("(")),
            separated_list0(ws(tag(",")), expr),
            ws(tag(")")),
        )(i3)?;
        cur = AstNode::Call {
            receiver: Box::new(cur),
            method,
            args,
            type_args: vec![],
        };
        i = i4;
    }
    Ok((i, cur))
}

// === Unary, arithmetic, comparison, logical (unchanged from previous) ===
fn unary(input: Input) -> Res<AstNode> {
    alt((
        method_call,
        map(preceded(ws(tag("-")), unary), |e| AstNode::Call {
            receiver: Box::new(e),
            method: "neg".to_string(),
            args: vec![],
            type_args: vec![],
        }),
    ))(input)
}

fn factor(input: Input) -> Res<AstNode> {
    let (i, init) = unary(input)?;
    let mut i = i;
    let mut left = init;
    while let Ok((i2, op)) = alt((ws(tag("*")), ws(tag("/")), ws(tag("%"))))(i) {
        let (i3, right) = unary(i2)?;
        left = AstNode::Call {
            receiver: Box::new(left),
            method: match op { "*" => "mul", "/" => "div", "%" => "rem", _ => unreachable!() }.to_string(),
            args: vec![right],
            type_args: vec![],
        };
        i = i3;
    }
    Ok((i, left))
}

fn term(input: Input) -> Res<AstNode> {
    let (i, init) = factor(input)?;
    let mut i = i;
    let mut left = init;
    while let Ok((i2, op)) = alt((ws(tag("+")), ws(tag("-"))))(i) {
        let (i3, right) = factor(i2)?;
        left = AstNode::Call {
            receiver: Box::new(left),
            method: if op == "+" { "add" } else { "sub" }.to_string(),
            args: vec![right],
            type_args: vec![],
        };
        i = i3;
    }
    Ok((i, left))
}

fn comparison(input: Input) -> Res<AstNode> {
    let (i, init) = term(input)?;
    let mut i = i;
    let mut left = init;
    while let Ok((i2, op)) = alt((
        ws(tag("==")), ws(tag("!=")), ws(tag("<=")),
        ws(tag(">=")), ws(tag("<")), ws(tag(">")),
    ))(i) {
        let (i3, right) = term(i2)?;
        left = AstNode::Call {
            receiver: Box::new(left),
            method: match op {
                "==" => "eq", "!=" => "ne", "<=" => "le",
                ">=" => "ge", "<" => "lt", ">" => "gt",
                _ => unreachable!(),
            }.to_string(),
            args: vec![right],
            type_args: vec![],
        };
        i = i3;
    }
    Ok((i, left))
}

pub fn expr(input: Input) -> Res<AstNode> {
    comparison(input)
}

// === Statements ===
fn let_stmt(input: Input) -> Res<AstNode> {
    let (i, _) = preceded(multispace0, tag("let"))(input)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = ws(tag("="))(i)?;
    let (i, e) = expr(i)?;
    let (i, _) = ws(tag(";"))(i)?;
    Ok((i, AstNode::Assign(name, Box::new(e))))
}

fn expr_stmt(input: Input) -> Res<AstNode> {
    let (i, e) = expr(input)?;
    let (i, _) = ws(tag(";"))(i)?;
    Ok((i, AstNode::Assign("_".to_string(), Box::new(e))))
}

// === spawn and await ===
fn spawn_expr(input: Input) -> Res<AstNode> {
    let (i, _) = ws(tag("spawn"))(i)?;
    let (i, actor_ty) = ident(i)?;
    let (i, args) = delimited(ws(tag("(")), separated_list0(ws(tag(",")), expr), ws(tag(")")))(i)?;
    Ok((i, AstNode::SpawnActor {
        actor_ty,
        init_args: args.iter().map(|a| a.to_string()).collect(),
    }))
}

fn await_expr(input: Input) -> Res<AstNode> {
    let (i, _) = ws(tag("await"))(i)?;
    let (i, future) = expr(i)?;
    Ok((i, AstNode::Await { expr: Box::new(future) }))
}

fn stmt(input: Input) -> Res<AstNode> {
    alt((
        let_stmt,
        expr_stmt,
        map(spawn_expr, |e| AstNode::Assign("_".to_string(), Box::new(e))),
        map(await_expr, |e| AstNode::Assign("_".to_string(), Box::new(e))),
    ))(input)
}

fn block(input: Input) -> Res<Vec<AstNode>> {
    delimited(ws(tag("{")), many0(stmt), ws(tag("}")))(input)
}

// === Actor definition ===
fn actor_field(input: Input) -> Res<(String, String)> {
    let (i, name) = ident(input)?;
    let (i, _) = ws(tag(":"))(i)?;
    let (i, ty) = ident(i)?;
    Ok((i, (name, ty)))
}

pub fn parse_actor(input: Input) -> Res<AstNode> {
    let (i, _) = ws(tag("actor"))(i)?;
    let (i, name) = ident(i)?;
    let (i, state) = delimited(
        ws(tag("{")),
        many0(preceded(multispace0, actor_field)),
        ws(tag("}")),
    )(i)?;

    Ok((i, AstNode::ActorDef {
        name,
        state,
        methods: vec![],
    }))
}

// === Async fn ===
fn async_fn(input: Input) -> Res<AstNode> {
    let (i, _) = ws(tag("async"))(i)?;
    let (i, _) = ws(tag("fn"))(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = ws(tag("("))(i)?;
    let (i, _) = ws(tag(")"))(i)?;
    let (i, body) = block(i)?;
    Ok((i, AstNode::AsyncFn {
        name,
        params: vec![],
        ret: "Future<i32>".to_string(),
        body,
    }))
}

// === Top level ===
pub fn parse_top(input: Input) -> Res<AstNode> {
    alt((
        parse_actor,
        async_fn,
        parse_func,
    ))(input)
}

pub fn parse_zeta(input: Input) -> Res<Vec<AstNode>> {
    delimited(multispace0, many0(parse_top), multispace0)(input)
}

fn ret_ty(input: Input) -> Res<String> {
    preceded(ws(tag("->")), ws(ident))(input)
}

pub fn parse_func(input: Input) -> Res<AstNode> {
    let (i, _) = preceded(multispace0, tag("fn"))(input)?;
    let (i, _) = multispace1(i)?;
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
            ret: ret.unwrap_or_else(|| "i32".to_string()),
            body,
            where_clause: None,
            attrs: vec![],
            ret_expr,
        },
    ))
}
