// src/parser.rs
use crate::ast::AstNode;
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, multispace0},
    combinator::map,
    sequence::{delimited, preceded},
    multi::many0,
    IResult,
};

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(preceded(multispace0, parse_func))(input)
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tag("fn")(input)?;
    let (i, _) = multispace0(i)?;
    let (i, name) = map(alpha1, |s: &str| s.to_string())(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = delimited(tag("("), multispace0, tag(")"))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = delimited(tag("{"), multispace0, tag("}"))(i)?;
    Ok((i, AstNode::FuncDef {
        name,
        generics: vec![],
        params: vec![],
        ret: "i32".to_string(),
        body: vec![],
        where_clause: None,
        attrs: vec![],
        ret_expr: None,
    }))
}
