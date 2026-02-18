// src/frontend/parser/top_level.rs
//! Top-level parser for Zeta language constructs (functions, concepts, impls, etc.).
//! Updated to correctly handle explicit `return` statements in function bodies
//! without accidentally discarding them during implicit-return extraction.
use super::expr::parse_full_expr;
use super::parser::{
    parse_generic_params, parse_ident, parse_path, parse_type, skip_ws_and_comments, ws,
};
use super::stmt::parse_block_body;
use crate::frontend::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt, value};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, terminated};

fn parse_param(input: &str) -> IResult<&str, (String, String)> {
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, ty) = if name == "self" {
        (input, "Self".to_string())
    } else {
        let (input, _) = ws(tag(":")).parse(input)?;
        ws(parse_type).parse(input)?
    };
    Ok((input, (name, ty)))
}

fn parse_use_statement(input: &str) -> IResult<&str, Vec<AstNode>> {
    let (input, _) = ws(tag("use")).parse(input)?;
    let (input, path) = ws(parse_path).parse(input)?;
    let (input, group_opt) = opt(preceded(
        ws(tag("::")),
        delimited(
            ws(tag("{")),
            terminated(
                separated_list0(
                    ws(tag(",")),
                    ws(alt((value("self".to_string(), tag("self")), parse_ident))),
                ),
                opt(ws(tag(","))),
            ),
            ws(tag("}")),
        ),
    ))
    .parse(input)?;
    let (input, _) = ws(tag(";")).parse(input)?;
    let nodes = if let Some(items) = group_opt {
        let mut ns = vec![];
        for item in items {
            if item == "self" {
                ns.push(AstNode::Use { path: path.clone() });
            } else {
                let mut p = path.clone();
                p.push(item);
                ns.push(AstNode::Use { path: p });
            }
        }
        ns
    } else {
        vec![AstNode::Use { path }]
    };
    Ok((input, nodes))
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (input, extern_opt) = opt(ws(tag("extern"))).parse(input)?;
    let (input, _) = ws(tag("fn")).parse(input)?;
    let (input, path) = ws(parse_path).parse(input)?;
    let name = path.join("::");
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (input, params) = delimited(
        ws(tag("(")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_param)),
            opt(ws(tag(","))),
        ),
        ws(tag(")")),
    )
    .parse(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_type))).parse(input)?;
    let (input, (body, ret_expr, single_line)) = if extern_opt.is_some() {
        let (input, _) = ws(tag(";")).parse(input)?;
        (input, (vec![], None, false))
    } else {
        alt((
            map(
                delimited(ws(tag("{")), parse_block_body, ws(tag("}"))),
                |mut b| {
                    let re = if let Some(AstNode::ExprStmt { .. }) = b.last() {
                        if let Some(AstNode::ExprStmt { expr }) = b.pop() {
                            Some(expr)
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    (b, re, false)
                },
            ),
            map(preceded(ws(tag("=")), ws(parse_full_expr)), |e| {
                (vec![], Some(Box::new(e)), true)
            }),
        ))
        .parse(input)?
    };
    let input = if single_line {
        let (i, _) = ws(tag(";")).parse(input)?;
        i
    } else {
        input
    };
    let generics: Vec<String> = generics_opt.unwrap_or_default();
    let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
    let ast = if extern_opt.is_some() {
        AstNode::ExternFunc { name, params, ret }
    } else {
        AstNode::FuncDef {
            name,
            generics,
            params,
            ret,
            body,
            attrs: vec![],
            ret_expr,
            single_line,
            doc: "".to_string(),
        }
    };
    Ok((input, ast))
}

pub fn parse_type_alias(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("type")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, ty) = ws(parse_type).parse(input)?;
    let (input, _) = ws(tag(";")).parse(input)?;
    Ok((input, AstNode::TypeAlias { name, ty }))
}

fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("concept")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (input, methods) =
        delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))).parse(input)?;
    let generics: Vec<String> = generics_opt.unwrap_or_default();
    Ok((
        input,
        AstNode::ConceptDef {
            name,
            generics,
            methods,
            doc: "".to_string(),
        },
    ))
}

fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("fn")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (input, params) = delimited(
        ws(tag("(")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_param)),
            opt(ws(tag(","))),
        ),
        ws(tag(")")),
    )
    .parse(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_type))).parse(input)?;
    let (input, _) = ws(tag(";")).parse(input)?;
    let generics: Vec<String> = generics_opt.unwrap_or_default();
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
    let (input, _) = ws(tag("impl")).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (input, concept) = ws(parse_ident).parse(input)?;
    let (input, _) = ws(tag("for")).parse(input)?;
    let (input, ty) = ws(parse_type).parse(input)?;
    let (input, body) =
        delimited(ws(tag("{")), many0(ws(parse_func)), ws(tag("}"))).parse(input)?;
    let generics: Vec<String> = generics_opt.unwrap_or_default();
    Ok((
        input,
        AstNode::ImplBlock {
            concept,
            generics,
            ty,
            body,
            doc: "".to_string(),
        },
    ))
}

fn parse_variant(input: &str) -> IResult<&str, (String, Vec<String>)> {
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, params_opt) = opt(alt((
        delimited(
            ws(tag("(")),
            terminated(
                separated_list0(ws(tag(",")), ws(parse_type)),
                opt(ws(tag(","))),
            ),
            ws(tag(")")),
        ),
        delimited(
            ws(tag("{")),
            terminated(
                separated_list0(ws(tag(",")), parse_struct_field),
                opt(ws(tag(","))),
            ),
            ws(tag("}")),
        )
        .map(|fields: Vec<(String, String)>| fields.into_iter().map(|(_, ty)| ty).collect()),
    )))
    .parse(input)?;
    let params = params_opt.unwrap_or_default();
    Ok((input, (name, params)))
}

fn parse_enum(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("enum")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, variants) = delimited(
        ws(tag("{")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_variant)),
            opt(ws(tag(","))),
        ),
        ws(tag("}")),
    )
    .parse(input)?;
    Ok((
        input,
        AstNode::EnumDef {
            name,
            variants,
            doc: "".to_string(),
        },
    ))
}

fn parse_struct_field(input: &str) -> IResult<&str, (String, String)> {
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, _) = ws(tag(":")).parse(input)?;
    let (input, ty) = ws(parse_type).parse(input)?;
    Ok((input, (name, ty)))
}

fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("struct")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, fields) = delimited(
        ws(tag("{")),
        terminated(
            separated_list0(ws(tag(",")), parse_struct_field),
            opt(ws(tag(","))),
        ),
        ws(tag("}")),
    )
    .parse(input)?;
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
    alt((
        parse_func,
        parse_type_alias,
        parse_concept,
        parse_impl,
        parse_enum,
        parse_struct,
    ))
    .parse(input)
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    let (input, _) = skip_ws_and_comments(input)?;
    let (input, vec_vec) = many0(ws(alt((
        parse_use_statement,
        map(parse_top_level_item, |node| vec![node]),
    ))))
    .parse(input)?;
    let asts: Vec<AstNode> = vec_vec.into_iter().flatten().collect::<Vec<AstNode>>();
    Ok((input, asts))
}
