// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.
//! Now with generics: fn name<T,U>(params) -> Ret { body }
//! Added hybrid traits: structural dispatch via method? (e.g., obj.add?(b) for ad-hoc structural lookup)
//! Added partial specialization: method::<T,U>(args) for type args in calls.
//! Updated Dec 13, 2025: Added f-string parsing (f"hello {expr}"); + as BinaryOp for str concat sugar.
//! Updated Dec 15, 2025: Use direct tuple syntax for nom 8.0 compatibility (tuple function deprecated); full expression parsing with precedence; proper f-string interpolation.

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{
    alpha1, alphanumeric0, char as nom_char, i64 as nom_i64, multispace0,
};
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

/// Whitespace wrapper for parsers.
fn ws<'a, O>(
    inner: impl Parser<&'a str, O, nom::error::Error<&'a str>>,
) -> impl Parser<&'a str, O, nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}

/// Parses an identifier (alpha + alphanum).
fn parse_ident<'a>() -> impl Parser<&'a str, String, nom::error::Error<&'a str>> {
    map(pair(alpha1, alphanumeric0), |(first, rest)| first.to_string() + rest)
}

/// Parses keyword.
#[allow(dead_code)]
fn parse_keyword<'a>(
    kw: &'static str,
) -> impl Parser<&'a str, (), nom::error::Error<&'a str>> {
    value((), ws(tag(kw)))
}

/// Parses an integer literal.
fn parse_literal<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    map(nom_i64, AstNode::Lit)
}

/// Parses string literal (r#"..."# stub as "..." for now).
fn parse_string_lit<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    map(
        delimited(tag("\""), take_while1(|c| c != '"'), tag("\"")),
        |s: &str| AstNode::StringLit(s.to_string()),
    )
}

/// Parses f-string: f"content {expr} more" - with proper interpolation.
fn parse_fstring<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    delimited(
        tag("f\""),
        many0(alt((
            map(take_while1(|c: char| c != '{' && c != '"'), |s: &str| AstNode::StringLit(s.to_string())),
            delimited(tag("{"), ws(parse_expr()), tag("}")),
        ))),
        tag("\""),
    )
    .map(AstNode::FString)
}

/// Parses a variable reference.
fn parse_variable<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    map(parse_ident(), AstNode::Var)
}

#[allow(dead_code)]
/// Parses path: A::B.
fn parse_path<'a>() -> impl Parser<&'a str, Vec<String>, nom::error::Error<&'a str>> {
    many1(preceded(opt(tag("::")), parse_ident()))
}

/// Parses primary: lit | var | str | fstr | TimingOwned (parentheses for precedence).
fn parse_primary<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    alt((
        parse_literal(),
        parse_string_lit(),
        parse_fstring(),
        parse_variable(),
        delimited(tag("("), ws(parse_expr()), tag(")")),
        parse_timing_owned(),
    ))
}

/// Parses postfix calls: primary (.method<type_args>?(args))*.
fn parse_postfix<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    map(
        pair(
            parse_primary(),
            many0(preceded(
                ws(tag(".")),
                map(
                    (parse_ident(), opt(ws(parse_type_args())), delimited(tag("("), many0(ws(parse_expr())), tag(")")), parse_structural()),
                    |(method, type_args_opt, args, structural)| (method, type_args_opt.unwrap_or(vec![]), args, structural),
                ),
            )),
        ),
        |(mut base, calls)| {
            for (method, type_args, args, structural) in calls {
                base = AstNode::Call {
                    receiver: Some(Box::new(base)),
                    method,
                    args,
                    type_args,
                    structural,
                };
            }
            base
        },
    )
}

/// Parses binary + with left-assoc chaining: postfix (+ postfix)*.
fn parse_binary_op<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    map(
        pair(
            parse_postfix(),
            many0(pair(ws(nom_char('+')), parse_postfix())),
        ),
        |(mut left, rights)| {
            for (_, right) in rights {
                left = AstNode::BinaryOp {
                    op: "+".to_string(),
                    left: Box::new(left),
                    right: Box::new(right),
                };
            }
            left
        },
    )
}

/// Parses TimingOwned<ty>(expr).
fn parse_timing_owned<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    map(
        (ws(tag("TimingOwned")), delimited(tag("<"), parse_ident(), tag(">")), tag("("), parse_expr(), tag(")")),
        |(_, ty, _, inner, _)| AstNode::TimingOwned { ty, inner: Box::new(inner) },
    )
}

/// Parses structural marker: ? after method name.
fn parse_structural<'a>() -> impl Parser<&'a str, bool, nom::error::Error<&'a str>> {
    map(opt(nom_char('?')), |opt_q| opt_q.is_some())
}

/// Parses type args: <T,U>.
fn parse_type_args<'a>() -> impl Parser<&'a str, Vec<String>, nom::error::Error<&'a str>> {
    delimited(
        tag("<"),
        separated_list1(tag(","), ws(parse_ident())),
        tag(">"),
    )
}

/// Parses generics: <T,U>.
fn parse_generics<'a>() -> impl Parser<&'a str, Vec<String>, nom::error::Error<&'a str>> {
    parse_type_args()
}

/// Parses assignment: var = expr.
fn parse_assign<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    map(
        (ws(parse_ident()), ws(tag("=")), ws(alt((parse_binary_op(), parse_primary())))),
        |(lhs, _, rhs)| AstNode::Assign(lhs, Box::new(rhs)),
    )
}

/// Parses defer: defer expr.
fn parse_defer<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    preceded(ws(tag("defer")), ws(alt((parse_binary_op(), parse_primary()))))
        .map(|node| AstNode::Defer(Box::new(node)))
}

/// Parses spawn: spawn func(args).
fn parse_spawn<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    map(
        (ws(tag("spawn")), ws(parse_ident()), delimited(tag("("), many0(ws(parse_primary())), tag(")"))),
        |(_, func, args)| AstNode::Spawn { func, args },
    )
}

/// Generic stmt parser.
fn parse_stmt<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    alt((
        parse_assign(),
        parse_defer(),
        parse_spawn(),
        parse_binary_op(),
        parse_primary(),
    ))
}

/// Parses function: fn name(gens) (params: types) -> ret { body }.
fn parse_func<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    let parse_fn_kw = value((), ws(tag("fn")));
    let parse_name = ws(parse_ident());
    let parse_generics_opt = opt(ws(parse_generics()));
    let parse_param_pair = pair(parse_ident(), preceded(ws(tag(":")), ws(parse_ident())));
    let parse_params = delimited(tag("("), many0(parse_param_pair), tag(")"));
    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_ident())));
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_stmt())), ws(tag("}")));

    map(
        (parse_fn_kw, parse_name, parse_generics_opt, parse_params, parse_ret, parse_body),
        |(_, name, generics_opt, params, ret_opt, body)| {
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
-> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    let parse_name = parse_ident();
    let parse_generics_opt = opt(ws(parse_generics()));
    let parse_params = delimited(tag("("), many0(parse_ident()), tag(")"));
    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_ident())));

    map(
        (parse_name, parse_generics_opt, parse_params, parse_ret),
        |(name, generics_opt, params, ret_opt)| AstNode::Method {
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
fn parse_concept<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("concept")));
    let parse_name = ws(parse_ident());
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig())), ws(tag("}")));

    map(
        (parse_kw, parse_name, parse_body),
        |(_, name, body)| AstNode::ConceptDef {
            name,
            methods: body,
        },
    )
}

/// Parses impl: impl concept for ty { methods }.
fn parse_impl<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("impl")));
    let parse_concept = ws(parse_ident());
    let parse_for_kw = value((), ws(tag("for")));
    let parse_ty = ws(parse_ident());
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig())), ws(tag("}")));

    map(
        (parse_kw, parse_concept, parse_for_kw, parse_ty, parse_body),
        |(_, concept, _, ty, body)| AstNode::ImplBlock { concept, ty, body },
    )
}

/// Parses enum: enum name { variants }.
fn parse_enum<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("enum")));
    let parse_name = ws(parse_ident());
    let parse_variants = delimited(ws(tag("{")), many0(ws(parse_ident())), ws(tag("}")));

    map(
        (parse_kw, parse_name, parse_variants),
        |(_, name, variants)| AstNode::EnumDef { name, variants },
    )
}

/// Parses struct: struct name { fields }.
fn parse_struct<'a>() -> impl Parser<&'a str, AstNode, nom::error::Error<&'a str>> {
    let parse_kw = value((), ws(tag("struct")));
    let parse_name = ws(parse_ident());
    let parse_field = map(
        (ws(parse_ident()), ws(tag(":")), ws(parse_ident())),
        |(n, _, t)| (n, t),
    );
    let parse_fields = delimited(ws(tag("{")), many0(parse_field), ws(tag("}")));

    map(
        (parse_kw, parse_name, parse_fields),
        |(_, name, fields)| AstNode::StructDef { name, fields },
    )
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
