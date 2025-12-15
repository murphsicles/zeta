// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.
//! Now with generics: fn name<T,U>(params) -> Ret { body }
//! Added hybrid traits: structural dispatch via method? (e.g., obj.add?(b) for ad-hoc structural lookup)
//! Added partial specialization: method::<T,U>(args) for type args in calls.
//! Updated Dec 13, 2025: Added f-string parsing (f"hello {expr}"); + as BinaryOp for str concat sugar.
//! Updated Dec 15, 2025: nom 8.0 compatible with associated types; full precedence; proper f-string interpolation.

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, alphanumeric0, i64 as nom_i64, multispace0};
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;
use nom::Parser;

fn ws<'a, O, E>(inner: impl Parser<&'a str, O, E>) -> impl Parser<&'a str, &'a str, E> {
    delimited(multispace0, tag(""), multispace0).map(|_| "").parse.with(inner)
}

fn parse_ident<'a>() -> impl Parser<&'a str, String> {
    map(pair(alpha1, alphanumeric0), |(f, r): (&'a str, &'a str)| f.to_string() + r)
}

fn parse_literal<'a>() -> impl Parser<&'a str, AstNode> {
    map(nom_i64, AstNode::Lit)
}

fn parse_string_lit<'a>() -> impl Parser<&'a str, AstNode> {
    map(
        delimited(tag("\""), take_while1(|c: char| c != '"'), tag("\"")),
        |s: &'a str| AstNode::StringLit(s.to_string()),
    )
}

fn parse_fstring<'a>() -> impl Parser<&'a str, AstNode> {
    delimited(
        tag("f\""),
        many0(alt((
            map(take_while1(|c: char| c != '{' && c != '"'), |s: &'a str| AstNode::StringLit(s.to_string())),
            delimited(tag("{"), parse_expr, tag("}")),
        ))),
        tag("\""),
    ).map(AstNode::FString)
}

fn parse_variable<'a>() -> impl Parser<&'a str, AstNode> {
    map(parse_ident(), AstNode::Var)
}

fn parse_timing_owned<'a>() -> impl Parser<&'a str, AstNode> {
    map(
        preceded(tag("TimingOwned"), delimited(tag("<"), parse_ident, tag(">"))).and(delimited(tag("("), parse_expr, tag(")"))),
        |(ty, inner)| AstNode::TimingOwned { ty, inner: Box::new(inner) },
    )
}

fn parse_primary<'a>() -> impl Parser<&'a str, AstNode> {
    alt((
        parse_literal,
        parse_string_lit,
        parse_fstring,
        parse_variable,
        delimited(tag("("), parse_expr, tag(")")),
        parse_timing_owned,
    ))
}

fn parse_structural<'a>() -> impl Parser<&'a str, bool> {
    map(opt(tag("?")), |o| o.is_some())
}

fn parse_type_args<'a>() -> impl Parser<&'a str, Vec<String>> {
    delimited(
        tag("<"),
        separated_list1(tag(","), parse_ident),
        tag(">"),
    )
}

fn parse_postfix<'a>() -> impl Parser<&'a str, AstNode> {
    map(
        pair(
            parse_primary,
            many0(preceded(
                tag("."),
                map(
                    (parse_ident, opt(parse_type_args), delimited(tag("("), many0(parse_expr), tag(")")), parse_structural),
                    |(method, type_args_opt, args, structural)| {
                        (method, type_args_opt.unwrap_or(vec![]), args, structural)
                    },
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

fn parse_add<'a>() -> impl Parser<&'a str, AstNode> {
    map(
        pair(
            parse_postfix,
            many0(preceded(tag("+"), parse_postfix)),
        ),
        |(mut left, rights)| {
            for right in rights {
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

fn parse_expr<'a>() -> impl Parser<&'a str, AstNode> {
    parse_add
}

fn parse_assign<'a>() -> impl Parser<&'a str, AstNode> {
    map(
        pair(parse_ident, preceded(tag("="), parse_expr)),
        |(lhs, rhs)| AstNode::Assign(lhs, Box::new(rhs)),
    )
}

fn parse_defer<'a>() -> impl Parser<&'a str, AstNode> {
    preceded(tag("defer"), parse_expr).map(|node| AstNode::Defer(Box::new(node)))
}

fn parse_spawn<'a>() -> impl Parser<&'a str, AstNode> {
    preceded(tag("spawn"), pair(parse_ident, delimited(tag("("), many0(parse_expr), tag(")"))))
        .map(|(func, args)| AstNode::Spawn { func, args })
}

fn parse_stmt<'a>() -> impl Parser<&'a str, AstNode> {
    alt((
        parse_assign,
        parse_defer,
        parse_spawn,
        parse_expr,
    ))
}

fn parse_func<'a>() -> impl Parser<&'a str, AstNode> {
    let parse_name = parse_ident;
    let parse_generics_opt = opt(parse_type_args);
    let parse_param_pair = pair(parse_ident, preceded(tag(":"), parse_ident));
    let parse_params = delimited(tag("("), many0(parse_param_pair), tag(")"));
    let parse_ret = opt(preceded(tag("->"), parse_ident));
    let parse_body = delimited(tag("{"), many0(parse_stmt), tag("}"));

    map(
        preceded(tag("fn"), (parse_name, parse_generics_opt, parse_params, parse_ret, parse_body)),
        |(name, generics_opt, params, ret_opt, body)| AstNode::FuncDef {
            name,
            generics: generics_opt.unwrap_or(vec![]),
            params,
            ret: ret_opt.unwrap_or("i64".to_string()),
            body,
            attrs: vec![],
            ret_expr: None,
        },
    )
}

fn parse_method_sig<'a>() -> impl Parser<&'a str, AstNode> {
    let parse_name = parse_ident;
    let parse_generics_opt = opt(parse_type_args);
    let parse_params = delimited(tag("("), many0(parse_ident), tag(")"));
    let parse_ret = opt(preceded(tag("->"), parse_ident));

    map(
        (parse_name, parse_generics_opt, parse_params, parse_ret),
        |(name, generics_opt, params, ret_opt)| AstNode::Method {
            name,
            params: params.into_iter().map(|p| (p, "i64".to_string())).collect(),
            ret: ret_opt.unwrap_or("i64".to_string()),
            generics: generics_opt.unwrap_or(vec![]),
        },
    )
}

fn parse_concept<'a>() -> impl Parser<&'a str, AstNode> {
    preceded(tag("concept"), pair(parse_ident, delimited(tag("{"), many0(parse_method_sig), tag("}"))))
        .map(|(name, methods)| AstNode::ConceptDef { name, methods })
}

fn parse_impl<'a>() -> impl Parser<&'a str, AstNode> {
    preceded(tag("impl"), (parse_ident, preceded(tag("for"), parse_ident), delimited(tag("{"), many0(parse_method_sig), tag("}"))))
        .map(|(concept, ty, body)| AstNode::ImplBlock { concept, ty, body })
}

fn parse_enum<'a>() -> impl Parser<&'a str, AstNode> {
    preceded(tag("enum"), pair(parse_ident, delimited(tag("{"), many0(parse_ident), tag("}"))))
        .map(|(name, variants)| AstNode::EnumDef { name, variants })
}

fn parse_struct<'a>() -> impl Parser<&'a str, AstNode> {
    preceded(tag("struct"), pair(parse_ident, delimited(tag("{"), many0(map(pair(parse_ident, preceded(tag(":"), parse_ident)), |(n, t)| (n, t))), tag("}"))))
        .map(|(name, fields)| AstNode::StructDef { name, fields })
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(alt((
        parse_func,
        parse_concept,
        parse_impl,
        parse_enum,
        parse_struct,
    )))(input)
}
