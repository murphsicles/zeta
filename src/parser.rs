// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, multispace0, multispace1},
    combinator::{map, opt, recognize},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

fn ident(input: &str) -> IResult<&str, String> {
    map(
        recognize(pair(
            alpha1,
            take_while(|c: char| c.is_alphanumeric() || c == '_'),
        )),
        String::from,
    )(input)
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(preceded(multispace0, top_level))(input)
}

fn top_level(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_concept,
        parse_impl,
        parse_struct,
        parse_actor,
        parse_func,
        parse_derive,
        parse_stable_abi_fn,
    ))(input)
}

fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("concept")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = multispace0(i)?;
    let (i, methods) = delimited(tag("{"), many0(preceded(multispace0, parse_method_sig)), tag("}"))(i)?;
    Ok((i, AstNode::ConceptDef { name, params: vec![], methods }))
}

fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("fn")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = delimited(tag("("), multispace0, tag(")"))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, ret) = opt(preceded(tag("->"), preceded(multispace0, ident)))(i)?;
    Ok((i, AstNode::Method { name, params: vec![("self".into(), "Self".into())], ret: ret.unwrap_or("Self".into()) }))
}

fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("impl")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, concept) = ident(i)?;
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("for")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, ty) = ident(i)?;
    let (i, _) = multispace0(i)?;
    let (i, body) = delimited(tag("{"), many0(preceded(multispace0, parse_method_sig)), tag("}"))(i)?;
    Ok((i, AstNode::ImplBlock { concept, ty, body }))
}

fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("struct")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = delimited(tag("("), many0(preceded(multispace0, ident)), tag(")"))(i)?;
    Ok((i, AstNode::StructDef { name, fields: vec![] }))
}

fn parse_actor(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("actor")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = delimited(tag("{"), many0(preceded(multispace0, parse_method_sig)), tag("}"))(i)?;
    Ok((i, AstNode::ActorDef { name, methods: vec![] }))
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("fn")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = delimited(tag("("), multispace0, tag(")"))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, ret) = opt(preceded(tag("->"), preceded(multispace0, ident)))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, body) = delimited(tag("{"), many0(preceded(multispace0, parse_stmt)), tag("}"))(i)?;
    Ok((
        i,
        AstNode::FuncDef {
            name,
            generics: vec![],
            params: vec![],
            ret: ret.unwrap_or("i32".into()),
            body,
            where_clause: None,
            attrs: vec![],
            ret_expr: None,
        },
    ))
}

fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_let,
        parse_call,
        parse_return_lit,
        parse_defer,
        parse_spawn,
        parse_timing_owned,
    ))(input)
}

fn parse_let(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("let")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, rhs) = parse_expr(i)?;
    Ok((i, AstNode::Let { name, rhs: Box::new(rhs) }))
}

fn parse_call(input: &str) -> IResult<&str, AstNode> {
    let (i, receiver) = ident(i)?;
    let (i, _) = tag(".")(i)?;
    let (i, method) = ident(i)?;
    let (i, _) = delimited(tag("("), many0(preceded(multispace0, ident)), tag(")"))(i)?;
    Ok((i, AstNode::Call { receiver, method, args: vec![] }))
}

fn parse_return_lit(input: &str) -> IResult<&str, AstNode> {
    map(
        preceded(tag("return"), preceded(multispace1, map(recognize(many1(alt((tag("0"), tag("1"), tag("2"), tag("3"), tag("4"), tag("5"), tag("6"), tag("7"), tag("8"), tag("9"))))), |s: &str| AstNode::Lit(s.parse().unwrap())))),
        |lit| lit,
    )(input)
}

fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("defer")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, stmt) = parse_stmt(i)?;
    Ok((i, AstNode::Defer(Box::new(stmt))))
}

fn parse_spawn(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("spawn_actor")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, actor_ty) = ident(i)?;
    Ok((i, AstNode::SpawnActor { actor_ty, init_args: vec![] }))
}

fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("TimingOwned")(i)?;
    let (i, _) = delimited(tag("<"), ident, tag(">"))(i)?;
    let (i, inner) = delimited(tag("("), parse_expr, tag(")"))(i)?;
    Ok((i, AstNode::TimingOwned { ty: "i32".into(), inner: Box::new(inner) }))
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    alt((parse_lit, parse_var))(input)
}

fn parse_lit(input: &str) -> IResult<&str, AstNode> {
    map(
        recognize(many1(alt((tag("0"), tag("1"), tag("2"), tag("3"), tag("4"), tag("5"), tag("6"), tag("7"), tag("8"), tag("9"))))),
        |s: &str| AstNode::Lit(s.parse().unwrap()),
    )(input)
}

fn parse_var(input: &str) -> IResult<&str, AstNode> {
    map(ident, AstNode::Var)(input)
}

fn parse_derive(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("#[derive(")(i)?;
    let (i, traits) = separated_list0(tag(","), preceded(multispace0, ident))(i)?;
    let (i, _) = tag(")]")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("struct")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, ty) = ident(i)?;
    Ok((i, AstNode::Derive { ty, traits }))
}

fn parse_stable_abi_fn(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("#[stable_abi]")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, func) = parse_func(i)?;
    if let AstNode::FuncDef { attrs, .. } = &func {
        let mut f = func.clone();
        if let AstNode::FuncDef { attrs: a, .. } = &mut f {
            a.push("stable_abi".into());
        }
        Ok((i, f))
    } else {
        Ok((i, func))
    }
}
