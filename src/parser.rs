// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.
//! Now with generics: fn name<T,U>(params) -> Ret { body }
//! Added hybrid traits: structural dispatch via method? (e.g., obj.add?(b) for ad-hoc structural lookup)
//! Added partial specialization: method::<T,U>(args) for type args in calls.
//! Updated Dec 13, 2025: Added f-string parsing (f"hello {expr}"); + as BinaryOp for str concat sugar.
//! Updated Dec 15, 2025: Switched to classic fn(input) -> IResult style for nom 8.0 compatibility and easy recursion; full precedence and interpolation.

use crate::ast::AstNode;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, alphanumeric0, i64 as nom_i64, multispace0};
use nom::combinator::{map, opt, call};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;

fn parse_ident(input: &str) -> IResult<&str, String> {
    map(pair(alpha1, alphanumeric0), |(f, r)| f.to_string() + r)(input)
}

fn parse_literal(input: &str) -> IResult<&str, AstNode> {
    map(nom_i64, AstNode::Lit)(input)
}

fn parse_string_lit(input: &str) -> IResult<&str, AstNode> {
    map(delimited(tag("\""), take_while1(|c: char| c != '"'), tag("\"")), |s| AstNode::StringLit(s.to_string()))(input)
}

fn parse_fstring(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("f\"")(input)?;
    let (input, parts) = many0(alt((
        map(take_while1(|c: char| c != '{' && c != '"'), |s| AstNode::StringLit(s.to_string())),
        delimited(tag("{"), delimited(multispace0, call(parse_expr), multispace0), tag("}")),
    )))(input)?;
    let (input, _) = tag("\"")(input)?;
    Ok(input, AstNode::FString(parts))
}

fn parse_variable(input: &str) -> IResult<&str, AstNode> {
    map(parse_ident, AstNode::Var)(input)
}

fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("TimingOwned")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ty) = delimited(tag("<"), parse_ident, tag(">"))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, inner) = call(parse_expr)(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag(")")(input)?;
    Ok(input, AstNode::TimingOwned { ty, inner: Box::new(inner) })
}

fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_literal,
        parse_string_lit,
        parse_fstring,
        parse_variable,
        delimited(tag("("), delimited(multispace0, call(parse_expr), multispace0), tag(")")),
        parse_timing_owned,
    ))(input)
}

fn parse_structural(input: &str) -> IResult<&str, bool> {
    map(opt(tag("?")), |o| o.is_some())(input)
}

fn parse_type_args(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tag("<"), separated_list1(tag(","), delimited(multispace0, parse_ident, multispace0)), tag(">"))(input)
}

fn parse_postfix(input: &str) -> IResult<&str, AstNode> {
    let (input, mut base) = parse_primary(input)?;
    let (input, calls) = many0(preceded(delimited(multispace0, tag("."), multispace0), 
        map(
            (parse_ident, opt(delimited(multispace0, parse_type_args, multispace0)), 
             delimited(tag("("), many0(delimited(multispace0, call(parse_expr), multispace0)), tag(")")), 
             delimited(multispace0, parse_structural, multispace0)),
            |(method, type_args_opt, args, structural)| (method, type_args_opt.unwrap_or(vec![]), args, structural),
        )
    ))(input)?;
    for (method, type_args, args, structural) in calls {
        base = AstNode::Call {
            receiver: Some(Box::new(base)),
            method,
            args,
            type_args,
            structural,
        };
    }
    Ok(input, base)
}

fn parse_add(input: &str) -> IResult<&str, AstNode> {
    let (input, mut left) = parse_postfix(input)?;
    let (input, rights) = many0(preceded(delimited(multispace0, tag("+"), multispace0), parse_postfix))(input)?;
    for right in rights {
        left = AstNode::BinaryOp {
            op: "+".to_string(),
            left: Box::new(left),
            right: Box::new(right),
        };
    }
    Ok(input, left)
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    parse_add(input)
}

fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    let (input, lhs) = delimited(multispace0, parse_ident, multispace0)(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, rhs) = delimited(multispace0, parse_expr, multispace0)(input)?;
    Ok(input, AstNode::Assign(lhs, Box::new(rhs)))
}

fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("defer")(input)?;
    let (input, inner) = delimited(multispace0, parse_expr, multispace0)(input)?;
    Ok(input, AstNode::Defer(Box::new(inner)))
}

fn parse_spawn(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("spawn")(input)?;
    let (input, func) = delimited(multispace0, parse_ident, multispace0)(input)?;
    let (input, args) = delimited(tag("("), many0(delimited(multispace0, parse_expr, multispace0)), tag(")"))(input)?;
    Ok(input, AstNode::Spawn { func, args })
}

fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_assign,
        parse_defer,
        parse_spawn,
        parse_expr,
    ))(input)
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("fn")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_ident(input)?;
    let (input, generics) = opt(delimited(multispace0, parse_type_args, multispace0))(input)?;
    let (input, params) = delimited(tag("("), many0(pair(delimited(multispace0, parse_ident, multispace0), preceded(tag(":"), delimited(multispace0, parse_ident, multispace0)))), tag(")"))(input)?;
    let (input, ret) = opt(preceded(delimited(multispace0, tag("->"), multispace0), parse_ident))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = delimited(tag("{"), many0(delimited(multispace0, parse_stmt, multispace0)), tag("}"))(input)?;
    Ok(input, AstNode::FuncDef {
        name,
        generics: generics.unwrap_or(vec![]),
        params,
        ret: ret.unwrap_or("i64".to_string()),
        body,
        attrs: vec![],
        ret_expr: None,
    })
}

// The other top-level parsers (concept, impl, enum, struct) similar to original, using the fn style.

fn parse_method_sig(input: &str) -> IResult<&str, AstNode> {
    let (input, name) = parse_ident(input)?;
    let (input, generics) = opt(delimited(multispace0, parse_type_args, multispace0))(input)?;
    let (input, params) = delimited(tag("("), many0(delimited(multispace0, parse_ident, multispace0)), tag(")"))(input)?;
    let (input, ret) = opt(preceded(delimited(multispace0, tag("->"), multispace0), parse_ident))(input)?;
    Ok(input, AstNode::Method {
        name,
        params: params.into_iter().map(|p| (p, "i64".to_string())).collect(),
        ret: ret.unwrap_or("i64".to_string()),
        generics: generics.unwrap_or(vec![]),
    })
}

fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("concept")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_ident(input)?;
    let (input, methods) = delimited(delimited(multispace0, tag("{"), multispace0), many0(delimited(multispace0, parse_method_sig, multispace0)), delimited(multispace0, tag("}"), multispace0))(input)?;
    Ok(input, AstNode::ConceptDef { name, methods })
}

fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("impl")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, concept) = parse_ident(input)?;
    let (input, _) = tag("for")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ty) = parse_ident(input)?;
    let (input, body) = delimited(delimited(multispace0, tag("{"), multispace0), many0(delimited(multispace0, parse_method_sig, multispace0)), delimited(multispace0, tag("}"), multispace0))(input)?;
    Ok(input, AstNode::ImplBlock { concept, ty, body })
}

fn parse_enum(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("enum")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_ident(input)?;
    let (input, variants) = delimited(delimited(multispace0, tag("{"), multispace0), many0(delimited(multispace0, parse_ident, multispace0)), delimited(multispace0, tag("}"), multispace0))(input)?;
    Ok(input, AstNode::EnumDef { name, variants })
}

fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("struct")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_ident(input)?;
    let (input, fields) = delimited(delimited(multispace0, tag("{"), multispace0), many0(map(
        (delimited(multispace0, parse_ident, multispace0), tag(":"), delimited(multispace0, parse_ident, multispace0)),
        |(n, _, t)| (n, t),
    )), delimited(multispace0, tag("}"), multispace0))(input)?;
    Ok(input, AstNode::StructDef { name, fields })
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(delimited(multispace0, alt((
        parse_func,
        parse_concept,
        parse_impl,
        parse_enum,
        parse_struct,
    )), multispace0))(input)
}
