// src/parser.rs
//! Nom-based parser for Zeta language syntax.
//! Handles top-level definitions including functions, concepts, implementations, enums, and structs.
//! Supports expressions such as literals, variables, calls, binary operations, timing-owned values, defer statements, spawn, returns, assignments, try propagations, dictionary literals, and subscripts.
//! Includes support for generics, structural dispatch, and partial specialization.
use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, alphanumeric0, i64 as nom_i64, multispace0};
use nom::combinator::{map, opt, value};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};
#[allow(dead_code)]
type FnParse = (
    (),
    String,
    Vec<String>,
    Vec<(String, String)>,
    Option<String>,
    Vec<AstNode>,
);
/// Wraps a parser to ignore surrounding whitespace.
fn ws<'a, F, O>(
    mut inner: F,
) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    F: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    move |input| {
        let (input, _) = multispace0(input)?;
        let (input, result) = inner.parse(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, result))
    }
}
/// Parses an identifier: alphabetic start followed by alphanumerics.
fn parse_ident(input: &str) -> IResult<&str, String> {
    map(
        pair(alpha1, alphanumeric0),
        |(first, rest): (&str, &str)| first.to_string() + rest,
    )
    .parse(input)
}
/// Parses a specific keyword, ignoring whitespace.
#[allow(dead_code)]
fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw))).parse(input)
}
/// Parses an integer literal.
fn parse_literal(input: &str) -> IResult<&str, AstNode> {
    map(nom_i64, AstNode::Lit).parse(input)
}
/// Parses a string literal enclosed in double quotes.
fn parse_string_lit(input: &str) -> IResult<&str, AstNode> {
    map(
        delimited(tag("\""), take_while1(|c| c != '"'), tag("\"")),
        |s: &str| AstNode::StringLit(s.to_string()),
    )
    .parse(input)
}
/// Parses f-string content: text segments interleaved with expressions.
fn parse_fstring_content(input: &str) -> IResult<&str, Vec<AstNode>> {
    let mut i = input;
    let mut parts = vec![];
    if !i.starts_with("f\"") {
        return Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Tag,
        )));
    }
    i = &i[2..];
    while !i.is_empty() && !i.starts_with('"') {
        if i.starts_with('{') {
            if let Some(closing) = i.find('}') {
                let expr_str = &i[1..closing];
                let (_, expr) = parse_full_expr(expr_str)?;
                parts.push(expr);
                i = &i[closing + 1..];
            } else {
                break;
            }
        } else {
            let text_end = i.find(['{', '"']).unwrap_or(i.len());
            if text_end > 0 {
                let text = &i[..text_end];
                parts.push(AstNode::StringLit(text.to_string()));
            }
            i = &i[text_end..];
        }
    }
    if i.starts_with('"') {
        i = &i[1..];
    }
    Ok((i, parts))
}
/// Parses an f-string literal.
fn parse_fstring(input: &str) -> IResult<&str, AstNode> {
    map(parse_fstring_content, AstNode::FString).parse(input)
}
/// Parses a variable reference.
fn parse_variable(input: &str) -> IResult<&str, AstNode> {
    map(parse_ident, AstNode::Var).parse(input)
}
/// Parses a path: segments separated by ::.
#[allow(dead_code)]
fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    map(
        many1(preceded(opt(tag("::")), parse_ident)),
        |ids: Vec<String>| ids,
    )
    .parse(input)
}
/// Parses a dictionary literal: { key: value, ... }.
fn parse_dict_lit(input: &str) -> IResult<&str, AstNode> {
    map(
        delimited(
            ws(tag("{")),
            separated_list1(
                ws(tag(",")),
                pair(
                    ws(parse_full_expr),
                    preceded(ws(tag(":")), ws(parse_full_expr)),
                ),
            ),
            ws(tag("}")),
        ),
        |entries| AstNode::DictLit { entries },
    )
    .parse(input)
}
/// Parses primary expressions: literals, variables, strings, f-strings, dictionaries, or parenthesized expressions.
fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_literal,
        parse_string_lit,
        parse_fstring,
        parse_variable,
        parse_dict_lit,
        delimited(ws(tag("(")), ws(parse_full_expr), ws(tag(")"))),
    ))
    .parse(input)
}
/// Parses postfix expressions: primaries with optional subscripts, try propagations, or method calls.
fn parse_postfix_expr(input: &str) -> IResult<&str, AstNode> {
    let (mut input, mut base) = parse_primary(input)?;
    loop {
        if let Ok((i, _)) = ws(tag("?")).parse(input) {
            base = AstNode::TryProp {
                expr: Box::new(base),
            };
            input = i;
        } else if let Ok((i, index)) =
            delimited(ws(tag("[")), ws(parse_full_expr), ws(tag("]"))).parse(input)
        {
            base = AstNode::Subscript {
                base: Box::new(base),
                index: Box::new(index),
            };
            input = i;
        } else if let Ok((i, method)) = preceded(ws(tag(".")), ws(parse_ident)).parse(input) {
            let (i, type_args) = opt(ws(parse_generics)).parse(i)?;
            let (i, structural) = if let Ok((i, _)) = ws(tag("?")).parse(i) {
                (i, true)
            } else {
                (i, false)
            };
            let (i, args) = delimited(
                ws(tag("(")),
                separated_list1(ws(tag(",")), ws(parse_full_expr)),
                ws(tag(")")),
            )
            .parse(i)?;
            base = AstNode::Call {
                receiver: Some(Box::new(base)),
                method,
                args,
                type_args: type_args.unwrap_or_default(),
                structural,
            };
            input = i;
        } else {
            break;
        }
    }
    Ok((input, base))
}
/// Parses binary operations: postfix expressions connected by operators like +.
fn parse_binary_op(input: &str) -> IResult<&str, AstNode> {
    let (input, left) = parse_postfix_expr(input)?;
    let (input, pairs) = many0(pair(
        alt((ws(tag("+")), ws(tag("concat")))),
        parse_postfix_expr,
    ))
    .parse(input)?;
    let mut expr = left;
    for (op, right) in pairs {
        expr = AstNode::BinaryOp {
            op: op.to_string(),
            left: Box::new(expr),
            right: Box::new(right),
        };
    }
    Ok((input, expr))
}
/// Parses a free function call: name(args).
fn parse_free_call(input: &str) -> IResult<&str, AstNode> {
    let (input, method) = ws(parse_ident).parse(input)?;
    let (input, type_args) = opt(ws(parse_generics)).parse(input)?;
    let (input, structural) = if let Ok((input, _)) = ws(tag("?")).parse(input) {
        (input, true)
    } else {
        (input, false)
    };
    let (input, args) = delimited(
        ws(tag("(")),
        separated_list1(ws(tag(",")), ws(parse_full_expr)),
        ws(tag(")")),
    )
    .parse(input)?;
    Ok((
        input,
        AstNode::Call {
            receiver: None,
            method,
            args,
            type_args: type_args.unwrap_or_default(),
            structural,
        },
    ))
}
/// Parses a full expression: calls, binary ops, postfix, or timing-owned.
fn parse_full_expr(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_free_call,
        parse_binary_op,
        parse_timing_owned,
        parse_postfix_expr,
    ))
    .parse(input)
}
/// Parses a timing-owned expression: TimingOwned<Type>(expr).
fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("TimingOwned")).parse(input)?;
    let (input, ty) = delimited(tag("<"), parse_ident, tag(">")).parse(input)?;
    let (input, _) = tag("(").parse(input)?;
    let (input, inner) = parse_full_expr(input)?;
    let (input, _) = tag(")").parse(input)?;
    Ok((
        input,
        AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        },
    ))
}
/// Parses a defer statement.
fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("defer")).parse(input)?;
    let (input, inner) = parse_full_expr(input)?;
    Ok((input, AstNode::Defer(Box::new(inner))))
}
/// Parses a spawn expression: spawn func(args).
fn parse_spawn(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("spawn")).parse(input)?;
    let (input, func) = ws(parse_ident).parse(input)?;
    let (input, args) = delimited(
        tag("("),
        separated_list1(tag(","), ws(parse_full_expr)),
        tag(")"),
    )
    .parse(input)?;
    Ok((input, AstNode::Spawn { func, args }))
}
/// Parses a return statement.
fn parse_return(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("return")).parse(input)?;
    let (input, expr) = ws(parse_full_expr).parse(input)?;
    Ok((input, AstNode::Return(Box::new(expr))))
}
/// Parses a statement: assignment, defer, spawn, return, or expression.
fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_assign,
        parse_defer,
        parse_spawn,
        parse_return,
        parse_full_expr,
    ))
    .parse(input)
}
/// Parses an assignment: lhs = rhs.
fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    let (input, lhs) = ws(parse_postfix_expr).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, rhs) = ws(parse_full_expr).parse(input)?;
    Ok((input, AstNode::Assign(Box::new(lhs), Box::new(rhs))))
}
/// Parses generics: <Type, Type>.
fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        ws(tag("<")),
        separated_list1(ws(tag(",")), ws(parse_ident)),
        ws(tag(">")),
    )
    .parse(input)
}
/// Parses a function definition: fn name<gens>(params) -> Ret { body } or single-line = expr.
fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("fn"))).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, generics_opt) = opt(ws(parse_generics)).parse(input)?;
    let (input, params) = delimited(
        tag("("),
        many0(pair(parse_ident, preceded(ws(tag(":")), ws(parse_ident)))),
        tag(")"),
    )
    .parse(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_ident))).parse(input)?;
    let (input, (body, single_line)) = alt((
        map(
            delimited(ws(tag("{")), many0(ws(parse_stmt)), ws(tag("}"))),
            |b| (b, false),
        ),
        map(preceded(ws(tag("=")), ws(parse_stmt)), |s| (vec![s], true)),
    ))
    .parse(input)?;
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
        },
    ))
}
/// Parses a method signature: name<gens>(params) -> Ret.
pub fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
    let (input, name) = parse_ident(input)?;
    let (input, generics_opt) = opt(ws(parse_generics)).parse(input)?;
    let (input, params) = delimited(tag("("), many0(parse_ident), tag(")")).parse(input)?;
    let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_ident))).parse(input)?;
    Ok((
        input,
        AstNode::Method {
            name,
            params: params
                .into_iter()
                .map(|p| (p.clone(), "i64".to_string()))
                .collect(),
            ret: ret_opt.unwrap_or_else(|| "i64".to_string()),
            generics: generics_opt.unwrap_or_default(),
        },
    ))
}
/// Parses a concept definition: concept Name { methods }.
fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("concept"))).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, body) =
        delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))).parse(input)?;
    Ok((
        input,
        AstNode::ConceptDef {
            name,
            methods: body,
        },
    ))
}
/// Parses an implementation: impl Concept for Type { methods }.
fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("impl"))).parse(input)?;
    let (input, concept) = ws(parse_ident).parse(input)?;
    let (input, _) = value((), ws(tag("for"))).parse(input)?;
    let (input, ty) = ws(parse_ident).parse(input)?;
    let (input, body) =
        delimited(ws(tag("{")), many0(ws(parse_method_sig)), ws(tag("}"))).parse(input)?;
    Ok((input, AstNode::ImplBlock { concept, ty, body }))
}
/// Parses an enum definition: enum Name { Variant, ... }.
fn parse_enum(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("enum"))).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, variants) =
        delimited(ws(tag("{")), many0(ws(parse_ident)), ws(tag("}"))).parse(input)?;
    Ok((input, AstNode::EnumDef { name, variants }))
}
/// Parses a struct definition: struct Name { field: Type, ... }.
fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = value((), ws(tag("struct"))).parse(input)?;
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, fields) = delimited(
        ws(tag("{")),
        many0(map(
            pair(ws(parse_ident), preceded(ws(tag(":")), ws(parse_ident))),
            |(n, t)| (n, t),
        )),
        ws(tag("}")),
    )
    .parse(input)?;
    Ok((input, AstNode::StructDef { name, fields }))
}
/// Parses a Zeta program: sequence of top-level items.
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(ws(alt((
        parse_func,
        parse_concept,
        parse_impl,
        parse_enum,
        parse_struct,
    ))))
    .parse(input)
}
