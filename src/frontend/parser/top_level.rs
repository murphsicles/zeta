// src/frontend/parser/top_level.rs
//! Top-level parser for Zeta language constructs (functions, concepts, impls, etc.).
//! Updated to correctly handle explicit `return` statements in function bodies
//! without accidentally discarding them during implicit-return extraction.
#![allow(unused_variables)]
use super::expr::parse_full_expr;
use super::parser::{
    parse_attributes, parse_generic_params, parse_ident, parse_path, parse_type,
    skip_ws_and_comments, ws,
};
use super::stmt::parse_block_body;
use crate::frontend::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, not, opt, peek, value};

use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, terminated};

fn parse_param(input: &str) -> IResult<&str, (String, String)> {
    // Try to parse &self or &mut self first (must not be followed by :)
    let parse_self = alt((
        // &mut self (must not be followed by :)
        map(
            (ws(tag("&mut")), ws(tag("self")), peek(not(ws(tag(":"))))),
            |_| ("&mut self".to_string(), "Self".to_string()),
        ),
        // &self (must not be followed by :)
        map(
            (ws(tag("&")), ws(tag("self")), peek(not(ws(tag(":"))))),
            |_| ("&self".to_string(), "Self".to_string()),
        ),
        // self (without &, must not be followed by :)
        map((ws(tag("self")), peek(not(ws(tag(":"))))), |_| {
            ("self".to_string(), "Self".to_string())
        }),
    ));

    // Try regular parameter: name: type
    let parse_regular = map(
        (ws(parse_ident), ws(tag(":")), ws(parse_type)),
        |(name, _, ty)| (name, ty),
    );

    alt((parse_self, parse_regular)).parse(input)
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

/// Parse visibility modifier (pub keyword)
fn parse_visibility(input: &str) -> IResult<&str, bool> {
    let (input, pub_opt) = opt(ws(tag("pub"))).parse(input)?;
    Ok((input, pub_opt.is_some()))
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    // Parse attributes first
    let (input, attrs) = parse_attributes(input)?;

    // Parse visibility
    let (input, _pub_) = parse_visibility(input)?;

    let (input, const_opt) = opt(ws(tag("const"))).parse(input)?;
    let (input, async_opt) = opt(ws(tag("async"))).parse(input)?;
    let (input, extern_opt) = opt(ws(tag("extern"))).parse(input)?;
    let (input, _) = ws(tag("fn")).parse(input)?;
    let (input, path) = ws(parse_path).parse(input)?;
    let name = path.join("::");
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (lifetimes, type_generics) = generics_opt.unwrap_or((Vec::new(), Vec::new()));
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
    let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
    let ast = if extern_opt.is_some() {
        AstNode::ExternFunc { 
            name, 
            generics: type_generics,
            lifetimes,
            params, 
            ret 
        }
    } else {
        AstNode::FuncDef {
            name,
            generics: type_generics,
            lifetimes,
            params,
            ret,
            body,
            attrs,
            ret_expr,
            single_line,
            doc: "".to_string(),
            pub_: false,
            async_: async_opt.is_some(),
            const_: const_opt.is_some(),
        }
    };
    Ok((input, ast))
}

pub fn parse_type_alias(input: &str) -> IResult<&str, AstNode> {
    // Parse attributes
    let (input, _attrs) = parse_attributes(input)?;

    // Parse visibility
    let (input, pub_) = parse_visibility(input)?;

    let (input, _) = ws(tag("type")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, ty) = ws(parse_type).parse(input)?;
    let (input, _) = ws(tag(";")).parse(input)?;
    Ok((input, AstNode::TypeAlias { name, ty, pub_ }))
}

fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    // Parse attributes
    let (input, attrs) = parse_attributes(input)?;

    let (input, _) = ws(tag("concept")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (lifetimes, type_generics) = generics_opt.unwrap_or((Vec::new(), Vec::new()));
    let (input, methods) =
        delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))).parse(input)?;
    Ok((
        input,
        AstNode::ConceptDef {
            name,
            generics: type_generics,
            lifetimes,
            methods,
            attrs,
            doc: "".to_string(),
            pub_: false,
        },
    ))
}

fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
    // Parse attributes
    let (input, attrs) = parse_attributes(input)?;

    let (input, _) = ws(tag("fn")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (lifetimes, type_generics) = generics_opt.unwrap_or((Vec::new(), Vec::new()));
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
    let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
    Ok((
        input,
        AstNode::Method {
            name,
            params,
            ret,
            generics: type_generics,
            lifetimes,
            attrs,
            doc: "".to_string(),
        },
    ))
}

fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    // Parse attributes
    let (input, attrs) = parse_attributes(input)?;

    let (input, _) = ws(tag("impl")).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;

    // Try to parse as inherent impl: impl<Generics> Type { ... }
    let parse_result = alt((
        // Trait impl: impl<Generics> Concept for Type
        map(
            (ws(parse_ident), ws(tag("for")), ws(parse_type)),
            |(concept, _, ty)| (concept, ty),
        ),
        // Inherent impl: impl<Generics> Type
        map(ws(parse_type), |ty| ("".to_string(), ty)),
    ))
    .parse(input);

    let (input, (concept, ty)) = match parse_result {
        Ok((input, (concept, ty))) => (input, (concept, ty)),
        Err(e) => {
            return Err(e);
        }
    };
    let (input, body) =
        delimited(ws(tag("{")), many0(ws(parse_func)), ws(tag("}"))).parse(input)?;
    let (lifetimes, type_generics) = generics_opt.unwrap_or((Vec::new(), Vec::new()));
    Ok((
        input,
        AstNode::ImplBlock {
            concept,
            generics: type_generics,
            lifetimes,
            ty,
            body,
            attrs,
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
    // Parse attributes
    let (input, attrs) = parse_attributes(input)?;

    // Parse visibility
    let (input, pub_) = parse_visibility(input)?;

    let (input, _) = ws(tag("enum")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    // Parse generic parameters if present (e.g., <T> or <T, E>)
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (lifetimes, type_generics) = generics_opt.unwrap_or((Vec::new(), Vec::new()));
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
            generics: type_generics,
            lifetimes,
            variants,
            attrs,
            doc: "".to_string(),
            pub_,
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
    // Parse attributes
    let (input, attrs) = parse_attributes(input)?;

    // Parse visibility
    let (input, pub_) = parse_visibility(input)?;

    let (input, _) = ws(tag("struct")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    // Parse generic parameters if present
    let (input, generics_opt) = opt(ws(parse_generic_params)).parse(input)?;
    let (lifetimes, type_generics) = generics_opt.unwrap_or((Vec::new(), Vec::new()));
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
            generics: type_generics,
            lifetimes,
            fields,
            attrs,
            doc: "".to_string(),
            pub_,
        },
    ))
}

fn parse_const(input: &str) -> IResult<&str, AstNode> {
    // Parse attributes
    let (input, _attrs) = parse_attributes(input)?;

    // Parse visibility
    let (input, pub_) = parse_visibility(input)?;

    let (input, _) = ws(tag("const")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, _) = ws(tag(":")).parse(input)?;
    let (input, ty) = ws(parse_type).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, value) = ws(parse_full_expr).parse(input)?;
    let (input, _) = ws(tag(";")).parse(input)?;

    Ok((
        input,
        AstNode::ConstDef {
            name,
            ty,
            value: Box::new(value),
            pub_,
        },
    ))
}

fn parse_macro_def(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("macro_rules!")).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;

    // Parse the macro body (simplified - just capture everything between braces)
    let (input, _) = ws(tag("{")).parse(input)?;

    // Find matching closing brace
    let mut depth = 1;
    let mut pos = 0;
    let chars: Vec<char> = input.chars().collect();

    while depth > 0 && pos < chars.len() {
        match chars[pos] {
            '{' => depth += 1,
            '}' => depth -= 1,
            _ => {}
        }
        pos += 1;
    }

    if depth > 0 {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Eof,
        )));
    }

    let body: String = chars[..pos - 1].iter().collect(); // pos-1 to exclude the closing '}'
    let remaining = &input[pos..];

    Ok((
        remaining,
        AstNode::MacroDef {
            name,
            patterns: body,
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
        parse_const,
        parse_macro_def,
    ))
    .parse(input)
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    let (input, _) = skip_ws_and_comments(input)?;

    let parse_result = many0(ws(alt((
        parse_use_statement,
        map(parse_top_level_item, |node| vec![node]),
    ))))
    .parse(input);

    let (input, vec_vec) = match parse_result {
        Ok((i, v)) => (i, v),
        Err(e) => {
            return Err(e);
        }
    };

    let asts: Vec<AstNode> = vec_vec.into_iter().flatten().collect::<Vec<AstNode>>();

    Ok((input, asts))
}
