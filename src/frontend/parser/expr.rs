// src/frontend/parser/expr.rs
use super::parser::{
    parse_ident, parse_path, parse_type, parse_type_args, skip_ws_and_comments0, ws,
};

use super::pattern::parse_pattern;
use super::stmt::parse_block_body;
use crate::frontend::ast::{AstNode, MatchArm};
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::combinator::opt;
use nom::error::Error as NomError;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, preceded, terminated};

fn parse_float_lit(input: &str) -> IResult<&str, AstNode> {
    // Simple float parser: digits.digits
    // For v0.3.8, support basic floats without exponent

    let mut chars = input.chars().peekable();
    let mut pos = 0;
    let mut has_digit = false;

    // Parse integer part
    while let Some(&c) = chars.peek() {
        if c.is_ascii_digit() {
            has_digit = true;
            chars.next();
            pos += c.len_utf8();
        } else {
            break;
        }
    }

    if !has_digit {
        return Err(nom::Err::Error(NomError::new(
            input,
            nom::error::ErrorKind::Digit,
        )));
    }

    // Check for decimal point
    let mut has_decimal = false;
    if let Some(&c) = chars.peek()
        && c == '.'
    {
        has_decimal = true;
        chars.next();
        pos += c.len_utf8();

        // Parse fractional part (at least one digit)
        let mut has_fraction_digit = false;
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                has_fraction_digit = true;
                chars.next();
                pos += c.len_utf8();
            } else {
                break;
            }
        }

        if !has_fraction_digit {
            // Invalid: decimal point without digits
            return Err(nom::Err::Error(NomError::new(
                input,
                nom::error::ErrorKind::Digit,
            )));
        }
    }

    // Must have decimal to be a float (for v0.3.8, no exponent support yet)
    if !has_decimal {
        return Err(nom::Err::Error(NomError::new(
            input,
            nom::error::ErrorKind::Digit,
        )));
    }

    let float_str = &input[..pos];
    let remaining = &input[pos..];
    Ok((remaining, AstNode::FloatLit(float_str.to_string())))
}

pub fn parse_lit(input: &str) -> IResult<&str, AstNode> {
    // Try float first, then integer
    match parse_float_lit(input) {
        Ok(result) => {
            return Ok(result);
        }
        Err(_) => {
            // Not a float, try integer
        }
    }

    let (input, num) = take_while(|c: char| c.is_ascii_digit()).parse(input)?;
    if num.is_empty() {
        return Err(nom::Err::Error(NomError::new(
            input,
            nom::error::ErrorKind::Digit,
        )));
    }
    let value = num.parse::<i64>().unwrap_or(0);
    Ok((input, AstNode::Lit(value)))
}

fn parse_string_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("\"")(input)?;

    // Simple parser that handles escaped quotes
    let mut content = String::new();
    let mut chars = input.chars();
    let mut pos = 0;

    while let Some(c) = chars.next() {
        pos += c.len_utf8();

        if c == '\\' {
            // Handle escape
            if let Some(next_c) = chars.next() {
                pos += next_c.len_utf8();
                if next_c == '"' {
                    content.push('"');
                } else if next_c == '\\' {
                    content.push('\\');
                } else if next_c == 'n' {
                    content.push('\n');
                } else if next_c == 't' {
                    content.push('\t');
                } else if next_c == 'r' {
                    content.push('\r');
                } else {
                    // Keep both chars for unknown escape
                    content.push('\\');
                    content.push(next_c);
                }
            } else {
                content.push('\\');
            }
        } else if c == '"' {
            // End of string
            let remaining = &input[pos..];
            return Ok((remaining, AstNode::StringLit(content)));
        } else {
            content.push(c);
        }
    }

    // No closing quote found
    Err(nom::Err::Error(NomError::new(
        input,
        nom::error::ErrorKind::Tag,
    )))
}

fn parse_path_expr(input: &str) -> IResult<&str, AstNode> {
    let (input, path) = parse_path(input)?;
    if path.is_empty() {
        return Err(nom::Err::Error(NomError::new(
            input,
            nom::error::ErrorKind::Many0,
        )));
    }
    let method = path.join("::");

    // Check for macro call: ident! but NOT ident!=
    let (input, is_macro) = if input.starts_with("!") && !input.starts_with("!=") {
        // It's ! for macro, not != operator
        let (input, _) = ws(tag("!")).parse(input)?;
        (input, Some(()))
    } else {
        (input, None)
    };

    if is_macro.is_some() {
        let (input, delim_start) = ws(alt((tag("("), tag("["), tag("{")))).parse(input)?;
        let close = match delim_start {
            "(" => ")",
            "[" => "]",
            "{" => "}",
            _ => {
                return Err(nom::Err::Error(NomError::new(
                    delim_start,
                    nom::error::ErrorKind::Alt,
                )));
            }
        };
        let (input, args) = terminated(
            separated_list0(ws(tag(",")), ws(parse_expr)),
            opt(ws(tag(","))),
        )
        .parse(input)?;
        let (input, _) = ws(tag(close)).parse(input)?;
        Ok((input, AstNode::MacroCall { name: method, args }))
    } else {
        let (input, type_args_opt) =
            opt(ws(preceded(opt(tag("::")), parse_type_args))).parse(input)?;
        let type_args: Vec<String> = type_args_opt.unwrap_or_default();

        let (input, args_opt) = opt(delimited(
            ws(tag("(")),
            terminated(
                separated_list0(ws(tag(",")), ws(parse_expr)),
                opt(ws(tag(","))),
            ),
            ws(tag(")")),
        ))
        .parse(input)?;

        if let Some(args) = args_opt {
            // Check if this is a path call (e.g., Point::new(10, 20))
            // Path should have at least 2 segments for a path call
            if path.len() >= 2 {
                // Split into path and method
                let (method_path, method_name) = path.split_at(path.len() - 1);
                Ok((
                    input,
                    AstNode::PathCall {
                        path: method_path.to_vec(),
                        method: method_name[0].clone(),
                        args,
                    },
                ))
            } else {
                // Single segment path, create regular Call
                Ok((
                    input,
                    AstNode::Call {
                        receiver: None,
                        method,
                        args,
                        type_args,
                        structural: false,
                    },
                ))
            }
        } else {
            let (input, fields_opt) = opt(delimited(
                ws(tag("{")),
                terminated(
                    separated_list1(ws(tag(",")), ws(parse_field_expr)),
                    opt(ws(tag(","))),
                ),
                ws(tag("}")),
            ))
            .parse(input)?;

            if let Some(fields) = fields_opt {
                Ok((
                    input,
                    AstNode::StructLit {
                        variant: method,
                        fields,
                    },
                ))
            } else {
                Ok((input, AstNode::Var(method)))
            }
        }
    }
}

fn parse_field_expr(input: &str) -> IResult<&str, (String, AstNode)> {
    let (input, name) = ws(parse_ident).parse(input)?;
    let (input, colon) = opt(ws(tag(":"))).parse(input)?;
    let (input, expr) = if colon.is_some() {
        ws(parse_expr).parse(input)?
    } else {
        (input, AstNode::Var(name.clone()))
    };
    Ok((input, (name, expr)))
}

fn parse_closure(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("|")).parse(input)?;
    let (input, params) = separated_list0(ws(tag(",")), ws(parse_ident)).parse(input)?;
    let (input, _) = ws(tag("|")).parse(input)?;
    let (input, body) = ws(parse_expr).parse(input)?;
    Ok((
        input,
        AstNode::Closure {
            params,
            body: Box::new(body),
        },
    ))
}

fn parse_block(input: &str) -> IResult<&str, AstNode> {
    let (input, body) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    Ok((input, AstNode::Block { body }))
}

fn parse_unsafe_expr(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("unsafe")).parse(input)?;
    let (input, block) = parse_block(input)?;
    if let AstNode::Block { body } = block {
        Ok((input, AstNode::Unsafe { body }))
    } else {
        Ok((input, block))
    }
}

fn parse_condition(input: &str) -> IResult<&str, AstNode> {
    // Parse condition for if/while - stops at '{' or other block delimiters
    parse_expr_no_if(input)
}

fn parse_if(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("if")).parse(input)?;

    // Parse condition with special handling
    let (input, cond) = if let Ok((i, expr)) = parse_condition(input) {
        (i, expr)
    } else {
        // Fallback: parse any expression
        ws(parse_expr_no_if).parse(input)?
    };
    let (input, then) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    let (input, else_opt) = opt(preceded(
        ws(tag("else")),
        delimited(ws(tag("{")), parse_block_body, ws(tag("}"))),
    ))
    .parse(input)?;
    let else_: Vec<AstNode> = else_opt.unwrap_or(vec![]);
    Ok((
        input,
        AstNode::If {
            cond: Box::new(cond),
            then,
            else_,
        },
    ))
}

fn parse_tuple_or_paren(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("(")).parse(input)?;
    let (input, mut items) = terminated(
        separated_list0(ws(tag(",")), ws(parse_expr)),
        opt(ws(tag(","))),
    )
    .parse(input)?;
    let (input, _) = ws(tag(")")).parse(input)?;
    if items.len() == 1 {
        Ok((input, items.remove(0)))
    } else {
        Ok((input, AstNode::Tuple(items)))
    }
}

fn parse_array_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("[")).parse(input)?;
    let (input, items) = terminated(
        separated_list0(ws(tag(",")), ws(parse_expr)),
        opt(ws(tag(","))),
    )
    .parse(input)?;
    let (input, _) = ws(tag("]")).parse(input)?;
    Ok((input, AstNode::ArrayLit(items)))
}

fn parse_bool(input: &str) -> IResult<&str, AstNode> {
    alt((
        tag("true").map(|_| AstNode::Bool(true)),
        tag("false").map(|_| AstNode::Bool(false)),
    ))
    .parse(input)
}

fn parse_unary(input: &str) -> IResult<&str, AstNode> {
    // Check for "!" but NOT followed by "=" (which would be != operator)
    let (input, op_opt) = if input.starts_with("!") && !input.starts_with("!=") {
        // It's a unary !, not !=
        let (input, _) = tag("!")(input)?;
        (input, Some("!"))
    } else {
        // Try other unary operators
        opt(alt((tag("&mut"), tag("&"), tag("-"), tag("*")))).parse(input)?
    };

    let (input, expr) = if op_opt.is_some() {
        ws(parse_unary).parse(input)?
    } else {
        parse_primary(input)?
    };
    if let Some(op) = op_opt {
        Ok((
            input,
            AstNode::UnaryOp {
                op: op.to_string(),
                expr: Box::new(expr),
            },
        ))
    } else {
        Ok((input, expr))
    }
}

fn parse_simple_ident(input: &str) -> IResult<&str, AstNode> {
    // Simple identifier parser, doesn't try to parse paths or method calls
    let (input, ident) = parse_ident(input)?;
    Ok((input, AstNode::Var(ident)))
}

fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_tuple_or_paren,
        parse_lit,
        parse_string_lit,
        parse_match_expr,   // Try match expression before identifier
        parse_path_expr,    // Try full path parser (handles struct literals)
        parse_simple_ident, // Fall back to simple ident
        parse_array_lit,
        parse_bool,
        parse_closure,
        parse_block,
        parse_unsafe_expr,
        // parse_if_let and parse_if removed - they're not primary expressions
        // They should be parsed at a higher level
    ))
    .parse(input)
}

fn parse_postfix(input: &str) -> IResult<&str, AstNode> {
    let (mut input, mut expr) = parse_unary(input)?;
    loop {
        let dot_result = ws(tag(".")).parse(input);
        if let Ok((i, _)) = dot_result {
            let (j, field_or_method) = parse_ident(i)?;

            // Check for type arguments first (e.g., ::<i32>)
            let (j2, type_args_opt) =
                opt(ws(preceded(opt(tag("::")), parse_type_args))).parse(j)?;
            let type_args: Vec<String> = type_args_opt.unwrap_or_default();

            // Now check if this is a method call (has parentheses) or field access
            // Check if there's a '('
            let args_opt = if let Ok((after_paren, _)) = ws(tag("(")).parse(j2) {
                // Parse argument list
                // Check if we have ')' immediately (empty argument list)
                let close_paren_result = ws(tag(")")).parse(after_paren);
                if let Ok((after_paren2, _)) = close_paren_result {
                    (Some(vec![]), after_paren2)
                } else {
                    // Parse non-empty argument list
                    let (after_args, args) = terminated(
                        separated_list1(ws(tag(",")), ws(parse_expr)),
                        opt(ws(tag(","))),
                    )
                    .parse(after_paren)?;

                    // Parse closing ')'
                    let (k, _) = ws(tag(")")).parse(after_args)?;
                    (Some(args), k)
                }
            } else {
                (None, j2)
            };

            let (args_opt_result, k) = args_opt;

            if let Some(args) = args_opt_result {
                // It's a method call (with or without type arguments)
                expr = AstNode::Call {
                    receiver: Some(Box::new(expr)),
                    method: field_or_method,
                    args,
                    type_args,
                    structural: false,
                };
                input = k;
            } else {
                // It's field access (type arguments would be invalid here, but we parsed them anyway)
                // For now, we'll ignore type_args for field access
                expr = AstNode::FieldAccess {
                    base: Box::new(expr),
                    field: field_or_method,
                };
                input = k;
            }
        } else if let Ok((i, index)) =
            delimited(ws(tag("[")), ws(parse_expr), ws(tag("]"))).parse(input)
        {
            expr = AstNode::Subscript {
                base: Box::new(expr),
                index: Box::new(index),
            };
            input = i;
        } else if let Ok((i, args)) = delimited(
            ws(tag("(")),
            terminated(
                separated_list0(ws(tag(",")), ws(parse_expr)),
                opt(ws(tag(","))),
            ),
            ws(tag(")")),
        )
        .parse(input)
        {
            expr = AstNode::Call {
                receiver: Some(Box::new(expr)),
                method: "call".to_string(),
                args,
                type_args: vec![],
                structural: false,
            };
            input = i;
        } else if let Ok((i, _)) = ws(tag("as")).parse(input) {
            let (j, ty) = ws(parse_type).parse(i)?;
            expr = AstNode::Cast {
                expr: Box::new(expr),
                ty,
            };
            input = j;
        } else {
            break;
        }
    }
    if let Ok((i, _)) = ws(tag("?")).parse(input) {
        expr = AstNode::TryProp {
            expr: Box::new(expr),
        };
        input = i;
    }
    if let Ok((i, _)) = ws(tag(".await")).parse(input) {
        expr = AstNode::Await(Box::new(expr));
        input = i;
    }
    Ok((input, expr))
}

// Parse expression without if (for use in if conditions to avoid left recursion)
fn parse_expr_no_if(input: &str) -> IResult<&str, AstNode> {
    let (mut input, mut term) = parse_postfix(input)?;
    loop {
        // Try to parse operator
        let mut found_op = None;
        let mut remaining_input = input;

        let operators = [
            "!=", "==", "<=", ">=", "<", ">", "+", "-", "*", "/", "%", "&&", "||", "..",
        ];

        // Try operators without whitespace first
        for &op in &operators {
            if remaining_input.starts_with(op) {
                found_op = Some(op);
                remaining_input = &remaining_input[op.len()..];
                break;
            }
        }

        // If no operator, try with whitespace
        if found_op.is_none() {
            let (i, _) = skip_ws_and_comments0(remaining_input).unwrap_or((remaining_input, ()));
            if i != remaining_input {
                for &op in &operators {
                    if let Some(stripped) = i.strip_prefix(op) {
                        found_op = Some(op);
                        remaining_input = stripped;
                        break;
                    }
                }
            }
        }

        if let Some(op) = found_op {
            // Skip whitespace after operator
            let (j, _) = skip_ws_and_comments0(remaining_input).unwrap_or((remaining_input, ()));
            let (j, right) = parse_postfix(j)?;
            term = AstNode::BinaryOp {
                op: op.to_string(),
                left: Box::new(term),
                right: Box::new(right),
            };
            input = j;
        } else {
            break;
        }
    }
    Ok((input, term))
}

/// Parse a match expression: `match expr { pattern => expr, ... }`
fn parse_match_expr(input: &str) -> IResult<&str, AstNode> {
    // Parse "match" with optional whitespace
    let (input, _) = ws(tag::<_, _, nom::error::Error<&str>>("match")).parse(input)?;

    // Parse scrutinee
    let (input, scrutinee) = parse_expr(input)?;

    // Parse whitespace before brace
    let (input, _) = skip_ws_and_comments0(input)?;

    // Parse "{"
    let (input, _) = ws(tag::<_, _, nom::error::Error<&str>>("{")).parse(input)?;

    // Parse arms
    let mut arms = Vec::new();
    let mut current_input = input;

    while let Ok((next_input, arm)) = parse_match_arm(current_input) {
        arms.push(arm);
        current_input = next_input;

        // Check for comma or closing brace
        let (next_input, _) = skip_ws_and_comments0(current_input)?;
        if let Ok((next_input, _)) = tag::<_, _, nom::error::Error<&str>>(",").parse(next_input) {
            current_input = next_input;
            let (next_input, _) = skip_ws_and_comments0(current_input)?;
            current_input = next_input;
            continue;
        }

        // Check for closing brace
        let (next_input, _) = skip_ws_and_comments0(current_input)?;
        if let Ok((next_input, _)) = tag::<_, _, nom::error::Error<&str>>("}").parse(next_input) {
            return Ok((
                next_input,
                AstNode::Match {
                    scrutinee: Box::new(scrutinee),
                    arms,
                },
            ));
        }

        // No comma or closing brace - error
        break;
    }

    // Parse closing brace
    let (input, _) = tag::<_, _, nom::error::Error<&str>>("}")(current_input)?;

    Ok((
        input,
        AstNode::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        },
    ))
}

/// Parse a single match arm: `pattern => expr` or `pattern if guard => expr`
fn parse_match_arm(input: &str) -> IResult<&str, MatchArm> {
    // Parse pattern (supports variables, literals, struct patterns, etc.)
    let (input, pattern) = parse_pattern(input)?;

    let (input, _) = skip_ws_and_comments0(input)?;

    // Parse optional guard
    let (input, guard) = opt(preceded(
        terminated(
            ws(tag::<_, _, nom::error::Error<&str>>("if")),
            skip_ws_and_comments0,
        ),
        parse_expr,
    ))
    .parse(input)?;

    let (input, _) = skip_ws_and_comments0(input)?;

    // Parse arrow
    let (input, _) = ws(tag::<_, _, nom::error::Error<&str>>("=>")).parse(input)?;

    // Parse body expression
    let (input, body) = parse_expr(input)?;

    Ok((
        input,
        MatchArm {
            pattern: Box::new(pattern),
            guard: guard.map(Box::new),
            body: Box::new(body),
        },
    ))
}

pub fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    // Try if expression first
    match parse_if(input) {
        Ok((remaining, if_expr)) => return Ok((remaining, if_expr)),
        Err(_) => {
            // Not an if expression, continue
        }
    }

    // Otherwise parse normal expression
    parse_expr_no_if(input)
}

pub fn parse_full_expr(input: &str) -> IResult<&str, AstNode> {
    parse_expr(input)
}
