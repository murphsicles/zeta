// src/parser.rs
use crate::ast::AstNode;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Concept, Impl, Fn, Ident(String), Colon, Arrow, LParen, RParen, Comma, Eq, Semi, Lt, Gt,
    Where, For, Mut, BraceOpen, BraceClose, IntLit(i64), AiOpt,
}

fn identifier(input: &str) -> IResult<&str, Token> {
    let ident = recognize(tuple((alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_")))))));
    map(ident, |s: &str| Token::Ident(s.to_string()))(input)
}

fn int_literal(input: &str) -> IResult<&str, Token> {
    map(digit1, |s: &str| Token::IntLit(s.parse().unwrap()))(input)
}

fn keyword(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Concept, tag("concept")),
        value(Token::Impl, tag("impl")),
        value(Token::Fn, tag("fn")),
        value(Token::Where, tag("where")),
        value(Token::For, tag("for")),
        value(Token::Mut, tag("mut")),
        value(Token::AiOpt, tag("ai_opt")),
    ))(input)
}

fn symbol(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Colon, char(':')),
        value(Token::Arrow, tag("->")),
        value(Token::LParen, char('(')),
        value(Token::RParen, char(')')),
        value(Token::Comma, char(',')),
        value(Token::Eq, char('=')),
        value(Token::Semi, char(';')),
        value(Token::Lt, char('<')),
        value(Token::Gt, char('>')),
        value(Token::BraceOpen, char('{')),
        value(Token::BraceClose, char('}')),
    ))(input)
}

pub fn tokenize(input: &str) -> IResult<&str, Vec<Token>> {
    many0(terminated(alt((int_literal, identifier, keyword, symbol)), multispace0))(input)
}

fn map_opt(p: impl Fn(&str) -> IResult<&str, Token>) -> impl Fn(&str) -> IResult<&str, Option<String>> {
    move |input| {
        let (i, t) = p(input)?;
        match t {
            Token::Ident(s) => Ok((i, Some(s))),
            _ => Ok((i, None)),
        }
    }
}

fn parse_attrs(input: &str) -> IResult<&str, Vec<String>> {
    many0(preceded(tag("#["), terminated(map_opt(identifier), tag("]"))))(input)
}

pub fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((
        tag("concept"), multispace0, map_opt(identifier), multispace0,
        opt(delimited(tag("<"), separated_list1(tag(","), map_opt(identifier)), tag(">"))), multispace0,
        tag("{"), multispace0, many0(parse_method), tag("}"),
    ));
    let (i, (_, _, name, _, params_opt, _, _, _, methods, _)) = parser(input)?;
    Ok((i, AstNode::ConceptDef { name: name.unwrap_or_default(), params: params_opt.unwrap_or_default(), methods }))
}

fn parse_method(input: &str) -> IResult<&str, AstNode> {
    let param_parser = separated_pair(opt(tag("mut")), tag(":"), tuple((map_opt(identifier), multispace0, tag(":"), multispace0, map_opt(identifier))));
    let parser = tuple((
        tag("fn"), multispace0, map_opt(identifier), multispace0,
        delimited(tag("("), separated_list1(tag(","), param_parser), tag(")")), multispace0,
        tag("->"), multispace0, map_opt(identifier), multispace0, tag(";"),
    ));
    let (i, (_, _, name, _, params, _, _, _, ret, _, _)) = parser(input)?;
    let method_params: Vec<(String, String)> = params.into_iter().map(|(mut_opt, (pn, _, _, _, ty))| (pn.unwrap_or_default(), ty.unwrap_or_default())).collect();
    Ok((i, AstNode::Method { name: name.unwrap_or_default(), params: method_params, ret: ret.unwrap_or_default() }))
}

pub fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let ty_parser = tuple((map_opt(identifier), multispace0, opt(delimited(tag("<"), separated_list1(tag(","), map_opt(identifier)), tag(">")))));
    let parser = tuple((
        tag("impl"), multispace0, map_opt(identifier), multispace0, ty_parser.clone(), multispace0,
        tag("for"), multispace0, ty_parser, multispace0, tag("{"), multispace0, many0(parse_method), tag("}"),
    ));
    let (i, (_, _, concept, _, _, _, _, _, for_ty, _, _, _, methods, _)) = parser(input)?;
    let full_ty = if let Some(params) = for_ty.2 { format!("{}<{}>", for_ty.0.unwrap_or_default(), params.join(",")) } else { for_ty.0.unwrap_or_default() };
    Ok((i, AstNode::ImplBlock { concept: concept.unwrap_or_default(), ty: full_ty, body: methods }))
}

fn parse_call(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((map_opt(identifier), tag("."), map_opt(identifier), multispace0, delimited(tag("("), separated_list1(tag(","), map_opt(identifier)), tag(")")), multispace0, tag(";")));
    let (i, (receiver, _, method, _, args_opt, _, _)) = parser(input)?;
    Ok((i, AstNode::Call { method: method.unwrap_or_default(), receiver: receiver.unwrap_or_default(), args: args_opt.unwrap_or_default() }))
}

fn parse_borrow(input: &str) -> IResult<&str, AstNode> {
    preceded(tag("&"), map_opt(identifier).map(|s| AstNode::Borrow(s.unwrap_or_default())))(input)
}

fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((map_opt(identifier), tag("="), parse_expr, tag(";")));
    let (i, (var, _, expr, _)) = parser(input)?;
    Ok((i, AstNode::Assign(var.unwrap_or_default(), Box::new(expr))))
}

fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    preceded(tag("defer"), delimited(tag("{"), parse_expr, tag("}"))).map(|expr| AstNode::Defer(Box::new(expr)))(input)
}

fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((tag("TimingOwned"), tag("<"), map_opt(identifier), tag(">"), tag("("), parse_expr, tag(")")));
    let (i, (_, _, ty, _, _, expr, _)) = parser(input)?;
    Ok((i, AstNode::TimingOwned { ty: ty.unwrap_or_default(), inner: Box::new(expr) }))
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    alt((parse_call, parse_borrow, parse_timing_owned, parse_assign, parse_defer, map(int_literal, |t| AstNode::Lit(t.0)), map(identifier, |t| AstNode::Var(t.0))))(input)
}

fn parse_actor(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((
        tag("actor"), multispace0, map_opt(identifier), multispace0,
        tag("{"), multispace0, many0(alt((parse_async_method, parse_method))), tag("}"),
    ));
    let (i, (_, _, name, _, _, _, methods, _)) = parser(input)?;
    Ok((i, AstNode::ActorDef { name: name.unwrap_or_default(), methods }))
}

fn parse_async_method(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((
        tag("async"), multispace0, tag("fn"), multispace0, map_opt(identifier), multispace0,
        delimited(tag("("), separated_list1(tag(","), parse_param), tag(")")), multispace0,
        tag("->"), multispace0, map_opt(identifier), multispace0, tag(";"),
    ));
    let (i, (_, _, _, _, name, _, params, _, _, _, ret, _, _)) = parser(input)?;
    let method_params: Vec<(String, String)> = params.into_iter().map(|(pn, ty)| (pn.unwrap_or_default(), ty.unwrap_or_default())).collect();
    Ok((i, AstNode::Method { name: name.unwrap_or_default(), params: method_params, ret: ret.unwrap_or_default() }))
}

fn parse_param(input: &str) -> IResult<&str, (String, String)> {
    separated_pair(map_opt(identifier), tag(":"), map_opt(identifier))(input)
}

fn parse_spawn_actor(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((tag("spawn_actor"), multispace0, map_opt(identifier), multispace0, delimited(tag("("), separated_list1(tag(","), map_opt(identifier)), tag(")")), multispace0, tag(";")));
    let (i, (_, _, ty, _, args_opt, _, _)) = parser(input)?;
    Ok((i, AstNode::SpawnActor { actor_ty: ty.unwrap_or_default(), init_args: args_opt.unwrap_or_default() }))
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let generics_parser = opt(delimited(tag("<"), separated_list1(tag(","), map_opt(identifier)), tag(">")));
    let param_parser = separated_pair(opt(tag("mut")), tag(":"), tuple((map_opt(identifier), multispace0, tag(":"), multispace0, map_opt(identifier))));
    let parser = tuple((
        multispace0, parse_attrs,
        tag("fn"), multispace0, map_opt(identifier), multispace0, generics_parser, multispace0,
        delimited(tag("("), separated_list1(tag(","), param_parser), tag(")")), multispace0,
        tag("->"), multispace0, map_opt(identifier), multispace0, opt(parse_where_clause), multispace0,
        tag("{"), multispace0, many0(parse_expr), tag("}"),
    ));
    let (i, (_, attrs_opt, _, _, name, _, generics_opt, _, params, _, _, _, ret, _, where_opt, _, _, _, body, _)) = parser(input)?;
    let attrs = attrs_opt.unwrap_or_default();
    let generics = generics_opt.unwrap_or_default();
    let func_params: Vec<(String, String)> = params.into_iter().map(|(mut_opt, (pn, _, _, _, ty))| (pn.unwrap_or_default(), ty.unwrap_or_default())).collect();
    Ok((i, AstNode::FuncDef {
        name: name.unwrap_or_default(), generics, params: func_params, ret: ret.unwrap_or_default(),
        body, where_clause: where_opt, attrs,
    }))
}

fn parse_where_clause(input: &str) -> IResult<&str, Vec<(String, String)>> {
    preceded(tag("where"), separated_list1(tag(","), separated_pair(map_opt(identifier), tag(":"), map_opt(identifier))))(input)
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(alt((parse_func, parse_actor, parse_impl, parse_concept)))(input)
}
