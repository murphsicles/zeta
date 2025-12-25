// src/frontend/parser/top_level.rs
use crate::frontend::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list1};
use nom::sequence::{delimited, preceded};
use nom::{IResult, Parser};

use super::parser::{parse_ident, parse_keyword, parse_generics, ws};
use super::stmt::parse_stmt;

fn parse_param(input: &str) -> IResult<&str, (String, String)> {
    let (input, name) = parse_ident(input)?;
    let (input, _) = ws(tag(":")).parse(input)?;
    let (input, ty) = parse_ident(input)?;
    Ok((input, (name, ty)))
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = parse_keyword("fn")(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generics)).parse(input)?;
    let (input, params) = delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(parse_param)), ws(tag(")"))).parse(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_ident))).parse(input)?;
    let (input, (body, single_line)) = alt((
        map(delimited(ws(tag("{")), many0(ws(parse_stmt)), ws(tag("}"))), |b| (b, false)),
        map(preceded(ws(tag("=")), ws(parse_stmt)), |s| (vec![s], true)),
    )).parse(input)?;
    let generics = generics_opt.unwrap_or_default();
    let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
    Ok((
        input,
        AstNode::FuncDef {
            name,
            generics,
            params,
            ret,
            body,
            attrs: vec![],
            ret_expr: None,
            single_line,
            doc: "".to_string(),
        },
    ))
}

fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = parse_keyword("concept")(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generics)).parse(input)?;
    let (input, methods) = delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))).parse(input)?;
    let generics = generics_opt.unwrap_or_default();
    Ok((
        input,
        AstNode::ConceptDef {
            name,
            methods,
            doc: "".to_string(),
        },
    ))
}

fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = parse_keyword("fn")(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generics)).parse(input)?;
    let (input, params) = delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(parse_param)), ws(tag(")"))).parse(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_ident))).parse(input)?;
    let (input, _) = ws(tag(";")).parse(input)?;
    let generics = generics_opt.unwrap_or_default();
    let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
    Ok((
        input,
        AstNode::Method {
            name,
            generics,
            params,
            ret,
            doc: "".to_string(),
        },
    ))
}

fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = parse_keyword("impl")(input)?;
    let (input, concept) = ws(parse_ident).parse(input)?;
    let (input, _) = ws(tag("for")).parse(input)?;
    let (input, ty) = ws(parse_ident).parse(input)?;
    let (input, body) = delimited(ws(tag("{")), many0(ws(parse_stmt)), ws(tag("}"))).parse(input)?;
    Ok((
        input,
        AstNode::ImplBlock {
            concept,
            ty,
            body,
            doc: "".to_string(),
        },
    ))
}

fn parse_variant(input: &str) -> IResult<&str, (String, Vec<String>)> {
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, params_opt) = opt(delimited(ws(tag("(")), separated_list1(ws(tag(",")), ws(parse_ident)), ws(tag(")")))).parse(input)?;
    let params = params_opt.unwrap_or_default();
    Ok((input, (name, params)))
}

fn parse_enum(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = parse_keyword("enum")(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, variants) = delimited(ws(tag("{")), separated_list1(ws(tag(",")), ws(parse_variant)), ws(tag("}"))).parse(input)?;
    Ok((
        input,
        AstNode::EnumDef {
            name,
            variants,
            doc: "".to_string(),
        },
    ))
}

fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = parse_keyword("struct")(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, _) = ws(tag("{"))(input)?;
    let (input, fields) = many0(|i| {
        let (i, name) = ws(parse_ident).parse(i)?;
        let (i, _) = ws(tag(":")).parse(i)?;
        let (i, ty) = ws(parse_ident).parse(i)?;
        Ok((i, (name, ty)))
    }).parse(input)?;
    let (input, _) = ws(tag("}"))(input)?;
    Ok((
        input,
        AstNode::StructDef {
            name,
            fields,
            doc: "".to_string(),
        },
    ))
}

fn parse_top_level_item(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = multispace0(input)?;
    let (input, node) = alt((parse_func, parse_concept, parse_impl, parse_enum, parse_struct)).parse(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, node))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(parse_top_level_item).parse(input)
}
