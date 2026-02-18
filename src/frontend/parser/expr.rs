// src/frontend/parser/expr.rs
use super::parser::{parse_ident, parse_path, parse_type, parse_type_args, ws};
use super::stmt::{parse_block_body, parse_pattern};
use crate::frontend::ast::AstNode;
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::combinator::opt;
use nom::error::Error as NomError;
use nom::multi::{separated_list0, separated_list1}; // ← added separated_list1
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
    let (input, content) = take_while(|c: char| c != '"').parse(input)?;
    let (input, _) = tag("\"")(input)?;
    Ok((input, AstNode::StringLit(content.to_string())))
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

    let (input, is_macro) = opt(ws(tag("!"))).parse(input)?;
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
    let (input, expr) = ws(parse_full_expr).parse(input)?;
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

fn parse_if(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = ws(tag("if")).parse(input)?;
    let (input, cond) = ws(parse_full_expr).parse(input)?;
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
    let (input, op_opt) = opt(alt((tag("&mut"), tag("&"), tag("-"), tag("!")))).parse(input)?;
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

fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_lit,
        parse_string_lit,
        parse_path_expr,
        parse_closure,
        parse_block,
        parse_unsafe_expr,
        parse_if_let,
        parse_if,
        parse_tuple_or_paren,
        parse_array_lit,
        parse_bool,
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

pub fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    let (mut input, mut term) = parse_postfix(input)?;
    loop {
        let op_parser = ws(alt((
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("%"),
            tag("=="),
            tag("!="),
            tag("<"),
            tag(">"),
            tag("<="),
            tag(">="),
            tag("&&"),
            tag("||"),
            tag(".."),
        )));
        let mut p = opt(op_parser);
        let res = p.parse(input);
        if let Ok((i, Some(op))) = res {
            let (j, right) = ws(parse_postfix).parse(i)?;
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

pub fn parse_full_expr(input: &str) -> IResult<&str, AstNode> {
    parse_expr(input)
}
