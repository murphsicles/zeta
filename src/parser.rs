// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.
//! Now with generics: fn name<T,U>(params) -> Ret { body }
//! Added hybrid traits: structural dispatch via method? (e.g., obj.add?(b) for ad-hoc structural lookup)

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, alphanumeric0, i64 as nom_i64, multispace0, char as nom_char};
use nom::combinator::{map, opt, value};
use nom::multi::{many0, many1, separated_list};
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
fn parse_structural<'a>() -> impl Parser<&'a str, Output = bool, Error = nom::error::Error<&'a str>> {
    map(opt(nom_char('?')), |opt_q| opt_q.is_some())
}

/// Parses call: recv.method(args) or recv.method?(args) for structural.
fn parse_call<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    parse_base_expr()
        .and(ws(tag(".")))
        .and(parse_ident())
        .and(parse_structural())
        .and(delimited(tag("("), many0(ws(parse_base_expr())), tag(")")))
        .map(|((recv, _), (method, (structural, args)))| AstNode::Call {
            receiver: Some(Box::new(recv)),
            method,
            args,
            type_args: vec![],
            structural,  // New field for hybrid dispatch hint
        })
}

/// Parses path call: path :: method (args).
fn parse_path_call<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    map(
        (
            parse_path(),
            ws(tag("::")),
            parse_ident(),
            delimited(tag("("), many0(ws(parse_base_expr())), tag(")")),
        ),
        |(path, _, method, args)| AstNode::PathCall { path, method, args },
    )
}

/// Parses expression recursively.
fn parse_expr<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    parse_base_expr()
        .and(opt(parse_add()))
        .map(|(left, opt_add)| {
            if let Some((_, right)) = opt_add {
                AstNode::Call {
                    receiver: Some(Box::new(left)),
                    method: "add".to_string(),
                    args: vec![right],
                    type_args: vec![],
                    structural: false,  // Default nominal for +
                }
            } else {
                left
            }
        })
}

/// Parses defer: defer call.
fn parse_defer<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(tag("defer"))
        .and(alt((parse_call(), parse_expr())))
        .map(|(_, call)| AstNode::Defer(Box::new(call)))
}

/// Parses spawn: spawn func(args).
fn parse_spawn<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(tag("spawn"))
        .and(parse_ident())
        .and(delimited(tag("("), many0(ws(parse_base_expr())), tag(")")))
        .map(|((_, func), args)| AstNode::Spawn {
            func,
            args,
        })
}

/// Parses statement: expr | assign | defer | spawn.
fn parse_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(alt((
        parse_expr(),
        ws(parse_ident())
            .and(ws(tag("=")))
            .and(parse_expr())
            .map(|((name, _), expr)| AstNode::Assign(name, Box::new(expr))),
        parse_defer(),
        parse_spawn(),
        parse_call(),  // Allow calls as stmts
    )))
}

/// Parses generics: <T,U>.
fn parse_generics<'a>() -> impl Parser<&'a str, Output = Vec<String>, Error = nom::error::Error<&'a str>> {
    delimited(
        tag("<"),
        separated_list(tag(","), parse_ident()),
        tag(">"),
    )
}

/// Parses function: fn name<T,U> (params) -> ret { body }.
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
        .map(|(((((fn_kw, name), generics_opt), params), ret_opt), body)| {
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
        })
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
        .map(|((name, generics_opt), (params, ret_opt))| AstNode::Method {
            name,
            params: params
                .into_iter()
                .map(|p| (p.clone(), "i64".to_string()))
                .collect(),
            ret: ret_opt.unwrap_or_else(|| "i64".to_string()),
            generics: generics_opt.unwrap_or(vec![]),  // New: generics in methods
        })
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
        .map(|((((_, concept), _), ty), body)| AstNode::ImplBlock {
            concept,
            ty,
            body,
        })
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
        .map(|((_, name), fields)| AstNode::StructDef {
            name,
            fields,
        })
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
