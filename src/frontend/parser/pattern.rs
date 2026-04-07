// src/frontend/parser/pattern.rs
//! Module for parsing patterns in the Zeta language.

use super::expr::parse_lit;
use super::parser::{parse_ident, parse_path, skip_ws_and_comments, ws};
use crate::frontend::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::opt;
use nom::multi::separated_list0;
use nom::sequence::{delimited, preceded, terminated};

/// Parse a pattern: `_`, identifier, literal, tuple pattern, struct pattern, range pattern, bind pattern, or or-pattern.
pub fn parse_pattern(input: &str) -> IResult<&str, AstNode> {
    // First parse a basic pattern
    let (input, pattern) = alt((
        // Wildcard pattern
        tag("_").map(|_| AstNode::Ignore),
        // Tuple pattern: `(pattern, pattern, ...)`
        parse_tuple_pattern,
        // Struct pattern: `Path { field: pattern, ... }` or `Path(pattern, ...)`
        parse_struct_pattern,
        // Range pattern: `start..end` or `start..=end`
        parse_range_pattern,
        // Bind pattern: `ident @ pattern`
        parse_bind_pattern,
        // Or pattern: `pattern | pattern | ...`
        parse_or_pattern,
        // Literal pattern
        parse_lit,
        // Variable pattern
        parse_ident.map(AstNode::Var),
    ))
    .parse(input)?;
    
    // Then check for type annotation
    let (input, ty_opt) = opt(preceded(
        ws(tag(":")),
        ws(crate::frontend::parser::parser::parse_type),
    )).parse(input)?;
    
    if let Some(ty) = ty_opt {
        Ok((input, AstNode::TypeAnnotatedPattern {
            pattern: Box::new(pattern),
            ty,
        }))
    } else {
        Ok((input, pattern))
    }
}

/// Parse a tuple pattern: `(pattern, pattern, ...)`
fn parse_tuple_pattern(input: &str) -> IResult<&str, AstNode> {
    delimited(
        ws(tag("(")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_pattern)),
            opt(ws(tag(","))),
        ),
        ws(tag(")")),
    )
    .map(AstNode::Tuple)
    .parse(input)
}

/// Parse a struct pattern: either tuple struct `Path(pattern, ...)` or named struct `Path { field: pattern, ... }`
fn parse_struct_pattern(input: &str) -> IResult<&str, AstNode> {
    let (input, path) = parse_path(input)?;
    let variant = path.join("::");

    let (input, _) = skip_ws_and_comments(input)?;

    // Try tuple struct pattern first: `Path(pattern, ...)`
    if let Ok((_i, _)) = ws(tag("(")).parse(input) {
        return parse_tuple_struct_pattern(input, variant);
    }

    // Try named struct pattern: `Path { field: pattern, ... }`
    if let Ok((_i, _)) = ws(tag("{")).parse(input) {
        return parse_named_struct_pattern(input, variant);
    }

    // If neither, it's just a variable pattern with a path
    Ok((input, AstNode::Var(variant)))
}

/// Parse a tuple struct pattern: `Variant(pattern, pattern, ...)`
fn parse_tuple_struct_pattern(input: &str, variant: String) -> IResult<&str, AstNode> {
    let (input, pats) = delimited(
        ws(tag("(")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_pattern)),
            opt(ws(tag(","))),
        ),
        ws(tag(")")),
    )
    .parse(input)?;

    Ok((
        input,
        AstNode::StructPattern {
            variant,
            fields: pats
                .into_iter()
                .enumerate()
                .map(|(i, p)| (i.to_string(), p))
                .collect(),
            rest: false,
        },
    ))
}

/// Parse a named struct pattern: `Struct { field: pattern, ... }`
fn parse_named_struct_pattern(input: &str, variant: String) -> IResult<&str, AstNode> {
    let (input, fields) = delimited(
        ws(tag("{")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_field_pattern)),
            opt(ws(tag(","))),
        ),
        ws(tag("}")),
    )
    .parse(input)?;

    let (input, has_rest) = opt(preceded(ws(tag(",")), ws(tag("..")))).parse(input)?;

    Ok((
        input,
        AstNode::StructPattern {
            variant,
            fields,
            rest: has_rest.is_some(),
        },
    ))
}

/// Parse a field in a struct pattern: `field: pattern` or `field` (shorthand)
fn parse_field_pattern(input: &str) -> IResult<&str, (String, AstNode)> {
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, colon) = opt(ws(tag(":"))).parse(input)?;

    let (input, pat) = if colon.is_some() {
        // Field with explicit pattern: `field: pattern`
        ws(parse_pattern).parse(input)?
    } else {
        // Field shorthand: `field` is equivalent to `field: field`
        (input, AstNode::Var(name.clone()))
    };

    Ok((input, (name, pat)))
}

/// Parse a range pattern: `start..end` or `start..=end`
fn parse_range_pattern(input: &str) -> IResult<&str, AstNode> {
    let (input, start) = parse_lit(input)?;
    let (input, _) = ws(tag("..")).parse(input)?;
    let (input, inclusive): (_, Option<&str>) = opt(ws(tag("="))).parse(input)?;
    let (input, end) = parse_lit(input)?;
    
    Ok((input, AstNode::RangePattern {
        start: Box::new(start),
        end: Box::new(end),
        inclusive: inclusive.is_some(),
    }))
}

/// Parse a bind pattern: `ident @ pattern`
fn parse_bind_pattern(input: &str) -> IResult<&str, AstNode> {
    let (input, name) = parse_ident(input)?;
    let (input, _) = ws(tag("@")).parse(input)?;
    let (input, pattern) = parse_pattern(input)?;
    
    Ok((input, AstNode::BindPattern {
        name,
        pattern: Box::new(pattern),
    }))
}

/// Parse an or-pattern: `pattern | pattern | ...`
fn parse_or_pattern(input: &str) -> IResult<&str, AstNode> {
    let (input, first) = parse_simple_pattern(input)?;
    let (input, patterns) = nom::multi::many0(
        preceded(ws(tag("|")), ws(parse_simple_pattern))
    ).parse(input)?;
    
    // If there are no additional patterns, this isn't really an or-pattern
    if patterns.is_empty() {
        Ok((input, first))
    } else {
        let mut all_patterns = vec![first];
        all_patterns.extend(patterns);
        
        Ok((input, AstNode::OrPattern(all_patterns)))
    }
}

/// Parse a simple pattern (without | operator) for use in or-patterns
fn parse_simple_pattern(input: &str) -> IResult<&str, AstNode> {
    alt((
        tag("_").map(|_| AstNode::Ignore),
        parse_tuple_pattern,
        parse_struct_pattern,
        parse_range_pattern,
        parse_bind_pattern,
        parse_lit,
        parse_ident.map(AstNode::Var),
    ))
    .parse(input)
}
