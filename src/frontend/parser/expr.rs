// src/frontend/parser/expr.rs
use super::parser::{parse_ident, parse_path, parse_type, parse_type_args, ws, skip_ws_and_comments0};
use nom::combinator::map;
use super::stmt::{parse_block_body, parse_pattern};
use crate::frontend::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::combinator::opt;
use nom::error::Error as NomError;
use nom::multi::{separated_list0, separated_list1};
use nom::sequence::{delimited, preceded, terminated};

fn parse_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, num) = take_while(|c: char| c.is_digit(10)).parse(input)?;
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
    
    if let Some(_) = is_macro {
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

fn parse_if_let(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("if")).parse(input)?;
    let (input, _) = ws(tag("let")).parse(input)?;
    let (input, pattern) = ws(parse_pattern).parse(input)?;
    let (input, _) = ws(tag("=")).parse(input)?;
    let (input, expr) = ws(parse_expr_no_if).parse(input)?;
    let (input, then) = delimited(ws(tag("{")), parse_block_body, ws(tag("}"))).parse(input)?;
    let (input, else_opt) = opt(preceded(
        ws(tag("else")),
        delimited(ws(tag("{")), parse_block_body, ws(tag("}"))),
    ))
    .parse(input)?;
    let else_: Vec<AstNode> = else_opt.unwrap_or(vec![]);
    Ok((
        input,
        AstNode::IfLet {
            pattern: Box::new(pattern),
            expr: Box::new(expr),
            then,
            else_,
        },
    ))
}

fn parse_condition(input: &str) -> IResult<&str, AstNode> {
    // Parse condition for if/while - stops at '{' or other block delimiters
    parse_expr_no_if(input)
}

fn parse_if(input: &str) -> IResult<&str, AstNode> {
    println!("[PARSER DEBUG] parse_if called, input: {:?}", &input[..30.min(input.len())]);
    let (input, _) = ws(tag("if")).parse(input)?;
    println!("[PARSER DEBUG] parse_if: parsed 'if', remaining: {:?}", &input[..30.min(input.len())]);
    
    // Parse condition with special handling
    let (input, cond) = if let Ok((i, expr)) = parse_condition(input) {
        (i, expr)
    } else {
        // Fallback: parse any expression
        ws(parse_expr_no_if).parse(input)?
    };
    println!("[PARSER DEBUG] parse_if: parsed condition, remaining: {:?}", &input[..30.min(input.len())]);
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
    println!("[PARSER DEBUG] parse_unary called, input: {:?}", &input[..20.min(input.len())]);
    
    // Check for "!" but NOT followed by "=" (which would be != operator)
    let (input, op_opt) = if input.starts_with("!") && !input.starts_with("!=") {
        // It's a unary !, not !=
        let (input, _) = tag("!")(input)?;
        (input, Some("!"))
    } else {
        // Try other unary operators
        opt(alt((tag("&mut"), tag("&"), tag("-")))).parse(input)?
    };
    
    println!("[PARSER DEBUG] parse_unary: op_opt = {:?}, remaining: {:?}", op_opt, &input[..20.min(input.len())]);
    
    let (input, expr) = if op_opt.is_some() {
        ws(parse_primary).parse(input)?
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
    println!("[PARSER DEBUG] parse_primary called, input: {:?}", &input[..20.min(input.len())]);
    alt((
        parse_tuple_or_paren,
        parse_lit,
        parse_string_lit,
        parse_simple_ident,  // Try simple ident first
        parse_path_expr,     // Fall back to full path parser
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
        if let Ok((i, _)) = ws(tag(".")).parse(input) {
            let (j, method) = parse_ident(i)?;
            let (j, type_args_opt) = opt(ws(preceded(opt(tag("::")), parse_type_args))).parse(j)?;
            let (k, args) = delimited(
                ws(tag("(")),
                terminated(
                    separated_list0(ws(tag(",")), ws(parse_expr)),
                    opt(ws(tag(","))),
                ),
                ws(tag(")")),
            )
            .parse(j)?;
            let type_args: Vec<String> = type_args_opt.unwrap_or_default();
            expr = AstNode::Call {
                receiver: Some(Box::new(expr)),
                method,
                args,
                type_args,
                structural: false,
            };
            input = k;
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
    Ok((input, expr))
}

// Parse expression without if (for use in if conditions to avoid left recursion)
fn parse_expr_no_if(input: &str) -> IResult<&str, AstNode> {
    let (mut input, mut term) = parse_postfix(input)?;
    loop {
        // Try to parse operator
        let mut found_op = None;
        let mut remaining_input = input;
        
        let operators = ["!=", "==", "<=", ">=", "<", ">", "+", "-", "*", "/", "%", "&&", "||", ".."];
        
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
                    if i.starts_with(op) {
                        found_op = Some(op);
                        remaining_input = &i[op.len()..];
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

pub fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    println!("[PARSER DEBUG] parse_expr called, input first 50 chars: {:?}", &input[..50.min(input.len())]);
    
    // Try if expression first
    if let Ok((remaining, if_expr)) = parse_if(input) {
        return Ok((remaining, if_expr));
    }
    
    // Otherwise parse normal expression
    parse_expr_no_if(input)
}

pub fn parse_full_expr(input: &str) -> IResult<&str, AstNode> {
    parse_expr(input)
}
