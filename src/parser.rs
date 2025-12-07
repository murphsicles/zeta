// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.
//! Now with generics: fn name<T,U>(params) -> Ret { body }
//! Added hybrid traits: structural dispatch via method? (e.g., obj.add?(b) for ad-hoc structural lookup)
//! Added partial specialization: method::<T,U>(args) for type args in calls.

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{
    alpha1, alphanumeric0, char as nom_char, i64 as nom_i64, multispace0,
};
use nom::combinator::{map, opt, value};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, preceded};
use nom::{IResult, Parser};

type FnParse = (
    (),
    String,
    Vec<String>,
    Vec<(String, String)>,
    Option<String>,
    Vec<AstNode>,
);

/// Whitespace wrapper for parsers.
fn ws<'a, O>(
    inner: impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

/// Parses an identifier (alpha + alphanum).
fn parse_ident<'a>() -> impl Parser<&'a str, Output = String, Error = nom::error::Error<&'a str>> {
    map(alpha1.and(alphanumeric0), |(first, rest): (&str, &str)| {
        first.to_string() + rest
    })
}

/// Parses keyword.
#[allow(dead_code)]
fn parse_keyword<'a>(
    kw: &'static str,
) -> impl Parser<&'a str, Output = (), Error = nom::error::Error<&'a str>> {
    value((), ws(tag(kw)))
}

/// Parses an integer literal.
fn parse_literal<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    map(nom_i64, AstNode::Lit)
}

/// Parses string literal (r#"..."# stub as "..." for now).
fn parse_string_lit<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        delimited(tag("\""), take_while1(|c| c != '"'), tag("\"")),
        |s: &str| AstNode::StringLit(s.to_string()),
    )
}

/// Parses a variable reference.
fn parse_variable<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    map(parse_ident(), AstNode::Var)
}

/// Parses path: A::B.
fn parse_path<'a>() -> impl Parser<&'a str, Output = Vec<String>, Error = nom::error::Error<&'a str>>
{
    map(
        many1(preceded(opt(tag("::")), parse_ident())),
        |ids: Vec<String>| ids,
    )
}

/// Parses atom: lit | var | str.
fn parse_atom<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((parse_literal(), parse_string_lit(), parse_variable()))
}

/// Parses TimingOwned<ty>(atom).
fn parse_timing_owned<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(tag("TimingOwned"))
        .and(delimited(tag("<"), parse_ident(), tag(">")))
        .and(tag("("))
        .and(parse_atom())
        .and(tag(")"))
        .map(|((((_, ty), _), inner), _)| AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        })
}

/// Parses base expression: atom | TimingOwned.
fn parse_base_expr<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((parse_atom(), parse_timing_owned()))
}

/// Parses add: base + base (as binary add).
fn parse_add<'a>()
-> impl Parser<&'a str, Output = ((), AstNode), Error = nom::error::Error<&'a str>> {
    value((), ws(tag("+"))).and(parse_base_expr())
}

/// Parses structural marker: ? after method name.
fn parse_structural<'a>() -> impl Parser<&'a str, Output = bool, Error = nom::error::Error<&'a str>>
{
    map(opt(nom_char('?')), |opt_q| opt_q.is_some())
}

/// Parses type args: <T,U>.
fn parse_type_args<'a>()
-> impl Parser<&'a str, Output = Vec<String>, Error = nom::error::Error<&'a str>> {
    delimited(tag("<"), separated_list1(tag(","), parse_ident()), tag(">"))
}

/// Parses call: recv.method::<T,U>(args) or recv.method?(args) for structural.
fn parse_call<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let base = parse_base_expr()
        .and(ws(tag(".")))
        .and(parse_ident())
        .and(parse_structural())
        .and(opt(ws(parse_type_args())));
    base.and(delimited(tag("("), many0(ws(parse_base_expr())), tag(")")))
        .map(|(base_tuple, args)| {
            let ((((recv, _dot), method), structural), type_args_opt) = base_tuple;
            let type_args: Vec<String> = type_args_opt.unwrap_or(vec![]);
            AstNode::Call {
                receiver: Some(Box::new(recv)),
                method,
                args,
                type_args,
                structural,
            }
        })
}

/// Parses generics: <T,U>.
#[allow(dead_code)]
fn parse_generics<'a>() -> impl Parser<&'a str, Output = Vec<String>, Error = nom::error::Error<&'a str>> {
    delimited(tag("<"), separated_list1(tag(","), parse_ident()), tag(">"))
}

/// Parses assignment: var = expr.
fn parse_assign<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    parse_ident()
        .and(ws(tag("=")))
        .and(parse_base_expr())
        .map(|((name, _), expr)| AstNode::Assign(name, Box::new(expr)))
}

/// Parses defer: defer expr.
fn parse_defer<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(tag("defer"))
        .and(ws(parse_base_expr()))
        .map(|(_, inner)| AstNode::Defer(Box::new(inner)))
}

/// Parses spawn: spawn func(args).
fn parse_spawn<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(tag("spawn"))
        .and(parse_ident())
        .and(delimited(tag("("), many0(ws(parse_base_expr())), tag(")")))
        .map(|((_, func), args)| AstNode::Spawn { func, args })
}

/// Parses statement: assign | call | defer | spawn.
fn parse_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(alt((
        parse_assign(),
        parse_call(),
        parse_defer(),
        parse_spawn(),
    )))
}

/// Parses function: fn name(generics)(params) -> ret { body }.
fn parse_func<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_fn_kw = value((), ws(tag("fn")));
    let parse_name = ws(parse_ident());
    let parse_generics_opt = opt(ws(parse_generics()));
    let parse_param_pair = ws(parse_ident())
        .and(ws(tag(":")))
        .and(ws(parse_ident()))
        .map(|((n, _), t): ((String, &'a str), String)| (n, t));
    let parse_params = delimited(tag("("), many0(parse_param_pair), tag(")"));
    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_ident())));
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_stmt())), ws(tag("}")));

    parse_fn_kw
        .and(parse_name)
        .and(parse_generics_opt)
        .and(parse_params)
        .and(parse_ret)
        .and(parse_body)
        .map(
            |(((((_fn_kw, name), generics_opt), params), ret_opt), body)| {
                let generics = generics_opt.unwrap_or(vec![]);
                let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
                AstNode::FuncDef {
                    name,
                    generics,
                    params,
                    ret,
                    body,
                    attrs: vec![],
                    ret_expr: None,
                }
            },
        )
}

/// Parses method sig for concept/impl: name(params) -> ret, with optional generics.
fn parse_method_sig<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_name = parse_ident();
    let parse_generics_opt = opt(ws(parse_generics()));
    let parse_params = delimited(tag("("), many0(parse_ident()), tag(")"));
    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_ident())));

    parse_name
        .and(parse_generics_opt)
        .and(parse_params)
        .and(parse_ret)
        .map(
            |(((name, generics_opt), params), ret_opt)| AstNode::Method {
                name,
                params: params
                    .into_iter()
                    .map(|p| (p.clone(), "i64".to_string()))
                    .collect(),
                ret: ret_opt.unwrap_or_else(|| "i64".to_string()),
                generics: generics_opt.unwrap_or(vec![]), // New: generics in methods
            },
        )
}

/// Parses concept: concept name { methods }.
fn parse_concept<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    let parse_kw = value((), ws(tag("concept")));
    let parse_name = ws(parse_ident());
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig())), ws(tag("}")));

    parse_kw
        .and(parse_name)
        .and(parse_body)
        .map(|((_, name), body)| AstNode::ConceptDef {
            name,
            methods: body,
        })
}

/// Parses impl: impl concept for ty { methods }.
fn parse_impl<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("impl")));
    let parse_concept = ws(parse_ident());
    let parse_for_kw = value((), ws(tag("for")));
    let parse_ty = ws(parse_ident());
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig())), ws(tag("}")));

    parse_kw
        .and(parse_concept)
        .and(parse_for_kw)
        .and(parse_ty)
        .and(parse_body)
        .map(|((((_, concept), _), ty), body)| AstNode::ImplBlock { concept, ty, body })
}

/// Parses enum: enum name { variants }.
fn parse_enum<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("enum")));
    let parse_name = ws(parse_ident());
    let parse_variants = delimited(ws(tag("{")), many0(ws(parse_ident())), ws(tag("}")));

    parse_kw
        .and(parse_name)
        .and(parse_variants)
        .map(|((_, name), variants)| AstNode::EnumDef { name, variants })
}

/// Parses struct: struct name { fields }.
fn parse_struct<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    let parse_kw = value((), ws(tag("struct")));
    let parse_name = ws(parse_ident());
    let parse_field = ws(parse_ident())
        .and(ws(tag(":")))
        .and(ws(parse_ident()))
        .map(|((n, _), t): ((String, &'a str), String)| (n, t));
    let parse_fields = delimited(ws(tag("{")), many0(parse_field), ws(tag("}")));

    parse_kw
        .and(parse_name)
        .and(parse_fields)
        .map(|((_, name), fields)| AstNode::StructDef { name, fields })
}

/// Entry point: Parses multiple top-level items.
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(ws(alt((
        parse_func(),
        parse_concept(),
        parse_impl(),
        parse_enum(),
        parse_struct(),
    ))))
    .parse(input)
}
