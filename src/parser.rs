// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1, i64 as nom_i64, satisfy},
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded},
    IResult,
};

type Input<'a> = &'a str;
type Res<'a, O> = IResult<Input<'a>, O>;

// Whitespace wrapper
fn ws<'a, O>(inner: impl FnMut(Input<'a>) -> Res<'a, O>) -> impl FnMut(Input<'a>) -> Res<'a, O> {
    delimited(multispace0, inner, multispace0)
}

// Identifier parser
fn ident(input: Input) -> Res<String> {
    let first = satisfy(|c| c.is_alphabetic() || c == '_');
    let rest = satisfy(|c| c.is_alphanumeric() || c == '_');
    map((first, many0(rest)), |(f, r)| {
        let mut s = f.to_string();
        for c in r {
            s.push(c);
        }
        s
    })(input)
}

// Literals & variables
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

// Method calls
fn method_call(input: Input) -> Res<AstNode> {
    let (i, mut cur) = primary(input)?;
    let mut i = i;

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

// Arithmetic precedence
fn factor(input: Input) -> Res<AstNode> {
    let (i, mut left) = method_call(input)?;
    let mut i = i;

    while let Ok((i2, op)) = alt((ws(tag("*")), ws(tag("/")), ws(tag("%"))))(i) {
        let (i3, right) = method_call(i2)?;
        left = AstNode::Call {
            receiver: Box::new(left),
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

fn term(input: Input) -> Res<AstNode> {
    let (i, mut left) = factor(input)?;
    let mut i = i;

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

pub fn expr(input: Input) -> Res<AstNode> {
    term(input)
}

// Statements
fn let_stmt(input: Input) -> Res<AstNode> {
    preceded(
        ws(tag("let")),
        map(
            (ident, ws(tag("=")), expr, ws(tag(";"))),
            |(name, _, e, _)| AstNode::Assign(name, Box::new(e)),
        ),
    )(input)
}

fn expr_stmt(input: Input) -> Res<AstNode> {
    map((expr, ws(tag(";"))), |(e, _)| {
        AstNode::Assign("_".to_string(), Box::new(e))
    })(input)
}

// spawn / await
fn spawn_expr(input: Input) -> Res<AstNode> {
    let (i, _) = ws(tag("spawn"))(input)?;
    let (i, actor_ty) = ident(i)?;
    let (i, args) = delimited(ws(tag("(")), separated_list0(ws(tag(",")), expr), ws(tag(")")))(i)?;
    Ok((
        i,
        AstNode::SpawnActor {
            actor_ty,
            init_args: args.into_iter().map(|_| "tmp".to_string()).collect(), // placeholder
        },
    ))
}

fn await_expr(input: Input) -> Res<AstNode> {
    let (i, _) = ws(tag("await"))(input)?;
    let (i, future) = expr(i)?;
    Ok((i, AstNode::Await {
        expr: Box::new(future),
    }))
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

// Actor definition
fn actor_field(input: Input) -> Res<(String, String)> {
    map((ident, ws(tag(":")), ident), |(name, _, ty)| (name, ty))(input)
}

fn parse_actor(input: Input) -> Res<AstNode> {
    let (i, _) = ws(tag("actor"))(input)?;
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

// Async fn
fn async_fn(input: Input) -> Res<AstNode> {
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
            ret: "Future<i32>".to_string(),
            body,
        },
    ))
}

// Function definition
fn ret_ty(input: Input) -> Res<String> {
    preceded(ws(tag("->")), ws(ident))(input)
}

fn parse_func(input: Input) -> Res<AstNode> {
    let (i, _) = preceded(ws(tag("fn")), multispace1)(input)?;
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

// Top-level
fn parse_top(input: Input) -> Res<AstNode> {
    alt((parse_actor, async_fn, parse_func))(input)
}

pub fn parse_zeta(input: Input) -> Res<Vec<AstNode>> {
    delimited(multispace0, many0(parse_top), multispace0)(input)
}
