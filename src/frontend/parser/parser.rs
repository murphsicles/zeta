// src/frontend/parser/parser.rs
//! Top-level parser and common combinators for the Zeta language.
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while};
use nom::character::complete::{alpha1, multispace1, satisfy};
use nom::combinator::{map, opt, recognize, value, verify};

use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, terminated};

pub fn line_comment(input: &str) -> IResult<&str, ()> {
    value((), pair(tag("//"), take_while(|c| c != '\n' && c != '\r'))).parse(input)
}

pub fn block_comment(input: &str) -> IResult<&str, ()> {
    value((), delimited(tag("/*"), take_until("*/"), tag("*/"))).parse(input)
}

pub fn skip_ws_and_comments(input: &str) -> IResult<&str, ()> {
    value(
        (),
        many0(alt((value((), multispace1), line_comment, block_comment))),
    )
    .parse(input)
}

// New: skip whitespace but allow zero (for use in delimited)
pub fn skip_ws_and_comments0(input: &str) -> IResult<&str, ()> {
    value(
        (),
        many0(alt((value((), multispace1), line_comment, block_comment))),
    )
    .parse(input)
}

// New: require at least some whitespace or comment
pub fn skip_ws_and_comments1(input: &str) -> IResult<&str, ()> {
    value(
        (),
        many1(alt((value((), multispace1), line_comment, block_comment))),
    )
    .parse(input)
}

pub fn ws<'a, P>(
    inner: P,
) -> impl Parser<&'a str, Output = P::Output, Error = nom::error::Error<&'a str>>
where
    P: Parser<&'a str, Error = nom::error::Error<&'a str>>,
{
    delimited(skip_ws_and_comments0, inner, skip_ws_and_comments0)
}

pub fn parse_ident(input: &str) -> IResult<&str, String> {
    let (input, ident): (&str, &str) = verify(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(satisfy(|c: char| c.is_alphanumeric() || c == '_')),
        )),
        |s: &str| {
            ![
                "let", "mut", "if", "else", "for", "in", "loop", "unsafe", "return", "break",
                "continue", "fn", "concept", "impl", "enum", "struct", "type", "use", "extern",
                "dyn", "box", "as", "true", "false", "comptime", "const", "async", "pub",
                // TODO: re-add these when we implement logical operators
                // or when the self-hosted parser (parser.z) becomes the default
                // "and", "or", "not"
            ]
            .contains(&s)
        },
    )
    .parse(input)?;

    Ok((input, ident.to_string()))
}

pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    preceded(
        opt(ws(tag("::"))),
        separated_list1(ws(tag("::")), ws(parse_ident)),
    )
    .parse(input)
}

pub fn parse_type_path(input: &str) -> IResult<&str, String> {
    let (input, path) = parse_path(input)?;
    let (input, type_args_opt) = opt(parse_type_args).parse(input)?;
    let type_args: Vec<String> = type_args_opt.unwrap_or_default();
    let mut s = path.join("::");
    if !type_args.is_empty() {
        s.push('<');
        s.push_str(&type_args.join(", "));
        s.push('>');
    }
    Ok((input, s))
}

pub fn parse_tuple_type(input: &str) -> IResult<&str, String> {
    let (input, _) = ws(tag("(")).parse(input)?;
    let (input, items) = terminated(
        separated_list0(ws(tag(",")), ws(parse_type)),
        opt(ws(tag(","))),
    )
    .parse(input)?;
    let (input, _) = ws(tag(")")).parse(input)?;
    let s = if items.is_empty() {
        "()".to_string()
    } else if items.len() == 1 {
        items[0].clone()
    } else {
        format!("({})", items.join(", "))
    };
    Ok((input, s))
}

/// Parse array type with dual syntax support
///
/// Supports both:
/// 1. Zeta style: [T; N] (type before size) or [T] (unsized)
/// 2. PrimeZeta style: [N]T (size before type)
///
/// Returns: "[T; N]" format for consistency, or "[T]" for unsized
pub fn parse_array_type(input: &str) -> IResult<&str, String> {
    // Helper function for PrimeZeta style parsing
    fn parse_primezeta_array(input: &str) -> IResult<&str, (String, String)> {
        let (input, _) = ws(tag("[")).parse(input)?;
        // Parse size expression (could be digit or identifier)
        let (input, size) = ws(alt((
            // Numeric literal
            nom::character::complete::digit1.map(|s: &str| s.to_string()),
            // Identifier (for constants like NUM_RESIDUES)
            parse_ident,
        )))
        .parse(input)?;
        let (input, _) = ws(tag("]")).parse(input)?;
        // Parse element type
        let (input, elem_type) = ws(parse_type).parse(input)?;
        Ok((input, (size, elem_type)))
    }
    
    // First, try to parse as PrimeZeta style: [N]T
    // This is important because [10]i64 could be ambiguous
    match parse_primezeta_array(input) {
        Ok((remaining, (size, elem_type))) => {
            // Successfully parsed PrimeZeta style: [N]T
            return Ok((remaining, format!("[{}; {}]", elem_type, size)));
        }
        Err(_) => {
            // Not PrimeZeta style, try Zeta style
        }
    }

    // Try Zeta style: [T] or [T; N]
    let (input, _) = ws(tag("[")).parse(input)?;
    let (input, elem_type) = ws(parse_type).parse(input)?;

    // Check for optional size
    let (input, size_opt) = opt(preceded(
        ws(tag(";")),
        ws(alt((
            // Numeric literal
            nom::character::complete::digit1.map(|s: &str| s.to_string()),
            // Identifier
            parse_ident,
        ))),
    ))
    .parse(input)?;

    let (input, _) = ws(tag("]")).parse(input)?;

    let result = if let Some(size) = size_opt {
        format!("[{}; {}]", elem_type, size)
    } else {
        format!("[{}]", elem_type)
    };

    Ok((input, result))
}

pub fn parse_fn_type(input: &str) -> IResult<&str, String> {
    let (input, extern_opt) = opt(delimited(
        ws(tag("extern")),
        ws(tag("\"C\"")),
        ws(tag("fn")),
    ))
    .parse(input)?;
    let extern_str = if extern_opt.is_some() {
        "extern \"C\" "
    } else {
        ""
    };
    let (input, _) = ws(tag("fn")).parse(input)?;
    let (input, params) = delimited(
        ws(tag("(")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_type)),
            opt(ws(tag(","))),
        ),
        ws(tag(")")),
    )
    .parse(input)?;
    let (input, _) = ws(tag("->")).parse(input)?;
    let (input, ret) = ws(parse_type).parse(input)?;
    let params_str = params.join(", ");
    Ok((
        input,
        format!("{}{}({}) -> {}", extern_str, "fn", params_str, ret),
    ))
}

/// Parse pointer types: *const T, *mut T
pub fn parse_pointer_type(input: &str) -> IResult<&str, String> {
    let (input, pointer_type) = alt((
        ws(tag("*const")).map(|_| "*const"),
        ws(tag("*mut")).map(|_| "*mut"),
    ))
    .parse(input)?;

    let (input, inner_type) = ws(parse_type).parse(input)?;
    Ok((input, format!("{} {}", pointer_type, inner_type)))
}

pub fn parse_type(input: &str) -> IResult<&str, String> {
    let (mut input, mut s) = (input, String::new());

    // Parse references (can be multiple: &&T)
    loop {
        // Try to parse &mut
        if let Ok((i, _)) = ws(tag("&mut")).parse(input) {
            s.push_str("&mut ");
            input = i;
        }
        // Try to parse & with optional lifetime
        else if let Ok((i, _)) = ws(tag("&")).parse(input) {
            // Check for lifetime after &
            let (i, lifetime_opt) = opt(parse_lifetime_param).parse(i)?;

            s.push('&');
            if let Some(lifetime) = lifetime_opt {
                s.push_str(&lifetime);
                s.push(' ');
            }

            // Check for mut after lifetime
            let (i, is_mut) = opt(ws(tag("mut"))).parse(i)?;
            if is_mut.is_some() {
                s.push_str("mut ");
            }

            input = i;
        } else {
            break;
        }
    }

    let (input, base) = alt((
        tag("_").map(|_| "_".to_string()),
        parse_tuple_type,
        parse_fn_type,
        parse_array_type,
        parse_pointer_type,
        preceded(ws(tag("dyn")), ws(parse_type_path)).map(|p| format!("dyn {}", p)),
        // Built-in types
        tag("i64").map(|_| "i64".to_string()),
        tag("u64").map(|_| "u64".to_string()),
        tag("usize").map(|_| "usize".to_string()),
        tag("f64").map(|_| "f64".to_string()),
        tag("bool").map(|_| "bool".to_string()),
        tag("String").map(|_| "String".to_string()),
        parse_lt_type,
        parse_type_path,
    ))
    .parse(input)?;
    s += &base;
    Ok((input, s))
}

pub fn parse_type_args(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        ws(tag("<")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_type)),
            opt(ws(tag(","))),
        ),
        ws(tag(">")),
    )
    .parse(input)
}

/// Parse Zeta's lt() syntax for generic types: lt(Result, i64)
pub fn parse_lt_type(input: &str) -> IResult<&str, String> {
    let (input, _) = ws(tag("lt")).parse(input)?;
    let (input, _) = ws(tag("(")).parse(input)?;
    let (input, type_name) = ws(parse_ident).parse(input)?;
    let (input, type_args) = delimited(
        ws(tag(",")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_type)),
            opt(ws(tag(","))),
        ),
        ws(tag(")")),
    )
    .parse(input)?;

    if type_args.is_empty() {
        Ok((input, type_name))
    } else {
        let args_str = type_args.join(", ");
        Ok((input, format!("{}<{}>", type_name, args_str)))
    }
}

/// Parse a single lifetime parameter
/// Examples: "'a", "'static", "'lifetime"
pub fn parse_lifetime_param(input: &str) -> IResult<&str, String> {
    // Parse lifetime tick '
    let (input, _) = tag("'")(input)?;

    // Parse lifetime identifier
    let (input, lifetime_name) = parse_ident(input)?;

    Ok((input, format!("'{}", lifetime_name)))
}

/// Parse trait bounds: T: Clone + Display
pub fn parse_trait_bounds(input: &str) -> IResult<&str, Vec<String>> {
    let (input, _) = ws(tag(":")).parse(input)?;

    // Parse first bound
    let (input, first_bound) = ws(parse_type_path).parse(input)?;
    let mut bounds = vec![first_bound];

    // Parse additional bounds with +
    let mut input = input;
    while let Ok((new_input, _)) = ws(tag("+")).parse(input) {
        let (new_input, bound) = ws(parse_type_path).parse(new_input)?;
        bounds.push(bound);
        input = new_input;
    }

    Ok((input, bounds))
}

/// Parse where clause: where T: Clone, U: Debug
pub fn parse_where_clause(input: &str) -> IResult<&str, Vec<(String, Vec<String>)>> {
    let (input, _) = ws(tag("where")).parse(input)?;

    terminated(
        separated_list0(
            ws(tag(",")),
            ws(|input| {
                let (input, param) = parse_ident(input)?;
                let (input, bounds) = parse_trait_bounds(input)?;
                Ok((input, (param, bounds)))
            }),
        ),
        opt(ws(tag(","))),
    )
    .parse(input)
}

/// Parse a single generic parameter with optional trait bounds
/// Examples: "T", "T: Display", "T: Display + Debug"
pub fn parse_generic_param(input: &str) -> IResult<&str, String> {
    let (input, param_name) = ws(parse_ident).parse(input)?;

    // Check for trait bounds
    let (input, bounds_str) = if let Ok((input, bounds)) = parse_trait_bounds(input) {
        (input, format!(": {}", bounds.join(" + ")))
    } else {
        (input, String::new())
    };

    Ok((input, format!("{}{}", param_name, bounds_str)))
}

/// Parse generic parameters including both lifetime and type parameters
/// Examples: "<'a, T>", "<'a, 'b, T: Display, U>", "<T>"
pub fn parse_generic_params(input: &str) -> IResult<&str, (Vec<String>, Vec<String>)> {
    let (input, params) = delimited(
        ws(tag("<")),
        terminated(
            separated_list0(
                ws(tag(",")),
                ws(alt((
                    // Try to parse as lifetime parameter first
                    map(parse_lifetime_param, |p| (p, true)),
                    // Fall back to type parameter
                    map(parse_generic_param, |p| (p, false)),
                ))),
            ),
            opt(ws(tag(","))),
        ),
        ws(tag(">")),
    )
    .parse(input)?;

    // Separate lifetime parameters from type parameters
    let mut lifetimes = Vec::new();
    let mut type_params = Vec::new();

    for (param, is_lifetime) in params {
        if is_lifetime {
            lifetimes.push(param);
        } else {
            type_params.push(param);
        }
    }

    Ok((input, (lifetimes, type_params)))
}

/// Parse a single attribute (e.g., #[test] or #[derive(Clone, Debug)])
pub fn parse_attribute(input: &str) -> IResult<&str, String> {
    let (input, _) = tag("#[")(input)?;
    
    // Use a custom parser to handle nested brackets and strings
    let (input, content) = parse_attribute_content(input)?;
    
    let (input, _) = tag("]")(input)?;

    // Trim whitespace from the content
    let trimmed_content = content.trim();
    Ok((input, trimmed_content.to_string()))
}

/// Helper to parse attribute content, handling nested brackets and strings
fn parse_attribute_content(input: &str) -> IResult<&str, String> {
    let mut depth = 0;
    let mut in_string = false;
    let mut escape_next = false;
    let mut result = String::new();
    let mut chars = input.char_indices();
    
    while let Some((i, c)) = chars.next() {
        match c {
            '[' if !in_string => {
                depth += 1;
                result.push(c);
            }
            ']' if !in_string => {
                if depth == 0 {
                    return Ok((&input[i..], result));
                }
                depth -= 1;
                result.push(c);
            }
            '"' if !escape_next => {
                in_string = !in_string;
                result.push(c);
            }
            '\\' if in_string => {
                escape_next = true;
                result.push(c);
            }
            _ => {
                escape_next = false;
                result.push(c);
            }
        }
    }
    
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::TakeUntil)))
}

/// Parse zero or more attributes
pub fn parse_attributes(input: &str) -> IResult<&str, Vec<String>> {
    let mut attributes = Vec::new();
    let mut current_input = input;

    loop {
        // Skip whitespace and comments before checking for attribute
        let (input_after_ws, _) = skip_ws_and_comments0(current_input)?;

        // Check if we have an attribute
        if input_after_ws.starts_with("#[") {
            let (input_after_attr, attr) = parse_attribute(input_after_ws)?;
            attributes.push(attr);
            current_input = input_after_attr;
        } else {
            // No more attributes
            break;
        }
    }

    Ok((current_input, attributes))
}
