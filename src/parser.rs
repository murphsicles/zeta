// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1, i64 as nom_i64},
    combinator::{map, opt},
    multi::{many0, many1},
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
        for c in r {
            s.push(c);
        }
        s
    })(input)
}

// Primary expressions
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

// Method calls (left-associative)
fn method_call(input: Input) -> Res<AstNode> {
    let (mut i, mut expr) = primary(input)?;

    while let Ok((i2, _)) = ws(tag("."))(i) {
        let (i3, method) = ident(i2)?;
        let (i4, args) = delimited(
            ws(tag("(")),
            many0(delimited(multispace0, expr, multispace0)),
            ws(tag(")")),
        )(i3)?;
        expr = AstNode::Call {
            receiver: Box::new(expr),
            method,
            args,
            type_args: vec![],
        };
        i = i4;
    }
    Ok((i, expr))
}

// Unary operators
fn unary(input: Input) -> Res<AstNode> {
    alt((
        method_call,
        map(preceded(ws(tag("-")), unary), |e| AstNode::Call {
            receiver: Box::new(e),
            method: "neg".to_string(),
            args: vec![],
            type_args: vec![],
        }),
        map(preceded(ws(tag("!")), unary), |e| AstNode::Call {
            receiver: Box::new(e),
            method: "not".to_string(),
            args: vec![],
            type_args: vec![],
        }),
    ))(input)
}

// Multiplicative
fn mul_div(input: Input) -> Res<AstNode> {
    let (i, init) = unary(input)?;
    let mut i = i;
    let mut left = init;

    while let Ok((i2, op)) = alt((ws(tag("*")), ws(tag("/")), ws(tag("%"))))(i) {
        let (i3, right) = unary(i2)?;
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

// Additive
fn add_sub(input: Input) -> Res<AstNode> {
    let (i, init) = mul_div(input)?;
    let mut i = i;
    let mut left = init;

    while let Ok((i2, op)) = alt((ws(tag("+")), ws(tag("-"))))(i) {
        let (i3, right) = mul_div(i2)?;
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

// Comparison
fn comparison(input: Input) -> Res<AstNode> {
    let (i, init) = add_sub(input)?;
    let mut i = i;
    let mut left = init;

    while let Ok((i2, op)) = alt((
        ws(tag("==")), ws(tag("!=")), ws(tag("<=")),
        ws(tag(">=")), ws(tag("<")), ws(tag(">")),
    ))(i) {
        let (i3, right) = add_sub(i2)?;
        left = AstNode::Call {
            receiver: Box::new(left),
            method: match op {
                "==" => "eq",
                "!=" => "ne",
                "<=" => "le",
                ">=" => "ge",
                "<"  => "lt",
                ">"  => "gt",
                _ => unreachable!(),
            }.to_string(),
            args: vec![right],
            type_args: vec![],
        };
        i = i3;
    }
    Ok((i, left))
}

// Logical AND / OR (short-circuit aware in future codegen)
fn logical_and(input: Input) -> Res<AstNode> {
    let (i, init) = comparison(input)?;
    let mut i = i;
    let mut left = init;

    while let Ok((i2, _)) = ws(tag("&&"))(i) {
        let (i3, right) = comparison(i2)?;
        left = AstNode::Call {
            receiver: Box::new(left),
            method: "and".to_string(),
            args: vec![right],
            type_args: vec![],
        };
        i = i3;
    }
    Ok((i, left))
}

fn logical_or(input: Input) -> Res<AstNode> {
    let (i, init) = logical_and(input)?;
    let mut i = i;
    let mut left = init;

    while let Ok((i2, _)) = ws(tag("||"))(i) {
        let (i3, right) = logical_and(i2)?;
        left = AstNode::Call {
            receiver: Box::new(left),
            method: "or".to_string(),
            args: vec![right],
            type_args: vec![],
        };
        i = i3;
    }
    Ok((i, left))
}

pub fn expr(input: Input) -> Res<AstNode> {
    logical_or(input)
}

// Statements
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

fn stmt(input: Input) -> Res<AstNode> {
    alt((let_stmt, expr_stmt))(input)
}

fn block(input: Input) -> Res<Vec<AstNode>> {
    delimited(
        ws(tag("{")),
        many0(stmt),
        ws(tag("}")),
    )(input)
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

pub fn parse_zeta(input: Input) -> Res<Vec<AstNode>> {
    delimited(multispace0, many0(parse_func), multispace0)(input)
}
