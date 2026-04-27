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

use crate::frontend::ast::GenericParam;
use crate::frontend::parser::identity_type::parse_string_with_identity;

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
                "let", "mut", "if", "else", "for", "in", "loop", "while", "unsafe", "return", "break",
                "continue", "fn", "concept", "impl", "enum", "struct", "type", "use", "extern",
                "dyn", "box", "as", "true", "false", "comptime", "const", "async", "pub",
                // Built-in types - allow as identifiers so they can be used in paths like u64::MAX
                // The resolver/typechecker will reject invalid uses later.
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
    let result = preceded(
        opt(ws(tag("::"))),
        separated_list1(ws(tag("::")), ws(parse_ident)),
    )
    .parse(input);
    if let Ok((remaining, path)) = &result {
    }
    result
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
/// 3. Dynamic array: [dynamic]T (dynamic size array)
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
        let (input, elem_type) = ws(parse_non_array_type).parse(input)?;
        Ok((input, (size, elem_type)))
    }
    
    // Helper function for dynamic array parsing: [dynamic]T
    fn parse_dynamic_array(input: &str) -> IResult<&str, String> {
        let (input, _) = ws(tag("[")).parse(input)?;
        let (input, _) = ws(tag("dynamic")).parse(input)?;
        let (input, _) = ws(tag("]")).parse(input)?;
        let (input, elem_type) = ws(parse_non_array_type).parse(input)?;
        Ok((input, format!("[dynamic]{}", elem_type))) // Return as dynamic array
    }
    
    // Helper function for Zeta style parsing
    fn parse_zeta_array(input: &str) -> IResult<&str, String> {
        let (input, _) = ws(tag("[")).parse(input)?;
        let (input, elem_type) = ws(parse_non_array_type).parse(input)?;

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
    
    // Save the original input position
    let original_input = input;
    
    // First, try dynamic array (special case: [dynamic]T)
    match parse_dynamic_array(original_input) {
        Ok(result) => return Ok(result),
        Err(_) => {
            // Dynamic array failed, try PrimeZeta style first (for [limit]bool syntax)
            match parse_primezeta_array(original_input) {
                Ok((remaining, (size, elem_type))) => {
                    return Ok((remaining, format!("[{}; {}]", elem_type, size)));
                }
                Err(_) => {
                    // PrimeZeta style failed, try Zeta style
                    match parse_zeta_array(original_input) {
                        Ok(result) => return Ok(result),
                        Err(e) => {
                            // All failed, return the error
                            return Err(e);
                        }
                    }
                }
            }
        }
    }
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

/// Parse SIMD vector types: u64x8, f32x4, Vector<u64, 8>
pub fn parse_simd_type<'a>(input: &'a str) -> IResult<&'a str, String> {
    // Try shorthand syntax first: u64x8, f32x4, etc.
    let shorthand_parser = move |input: &'a str| -> IResult<&'a str, String> {
        // Parse base type: i8, i16, i32, i64, u8, u16, u32, u64, f32, f64
        let mut base_type_parser = alt((
            tag("i8"), tag("i16"), tag("i32"), tag("i64"),
            tag("u8"), tag("u16"), tag("u32"), tag("u64"),
            tag("f32"), tag("f64"),
        ));
        let (input, base_type) = base_type_parser.parse(input)?;
        
        // Parse 'x' separator
        let (input, _) = tag("x")(input)?;
        
        // Parse size (positive integer)
        let (input, size_str) = nom::character::complete::digit1(input)?;
        
        // Convert size to usize
        let size = size_str.parse::<usize>().unwrap_or(0);
        if size == 0 {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Digit,
            )));
        }
        
        Ok((input, format!("Vector<{}, {}>", base_type, size)))
    };
    
    // Try Vector<T, N> syntax
    let generic_parser = move |input: &'a str| -> IResult<&'a str, String> {
        let (input, _) = ws(tag("Vector")).parse(input)?;
        let (input, _) = ws(tag("<")).parse(input)?;
        
        // Parse element type
        let (input, elem_type) = ws(parse_type).parse(input)?;
        
        let (input, _) = ws(tag(",")).parse(input)?;
        
        // Parse size
        let (input, size_str) = ws(nom::character::complete::digit1).parse(input)?;
        let size = size_str.parse::<usize>().unwrap_or(0);
        if size == 0 {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Digit,
            )));
        }
        
        let (input, _) = ws(tag(">")).parse(input)?;
        
        Ok((input, format!("Vector<{}, {}>", elem_type, size)))
    };
    
    // Try shorthand first, then generic
    alt((shorthand_parser, generic_parser)).parse(input)
}

/// Parse a type that cannot be an array type (used to break recursion)
pub fn parse_non_array_type(input: &str) -> IResult<&str, String> {
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

    // Special types excluding array_type and tuple_type to avoid recursion
    // (tuple_type can contain array types)
    let special_types_no_array = alt((
        tag("_").map(|_| "_".to_string()),
        parse_fn_type,
        parse_pointer_type,
        preceded(ws(tag("dyn")), ws(parse_type_path)).map(|p| format!("dyn {}", p)),
        parse_simd_type,
        parse_lt_type,
    ));
    
    // Then try built-in types
    let builtin_types = alt((
        tag("i8").map(|_| "i8".to_string()),
        tag("i16").map(|_| "i16".to_string()),
        tag("i32").map(|_| "i32".to_string()),
        tag("i64").map(|_| "i64".to_string()),
        tag("u8").map(|_| "u8".to_string()),
        tag("u16").map(|_| "u16".to_string()),
        tag("u32").map(|_| "u32".to_string()),
        tag("u64").map(|_| "u64".to_string()),
        tag("usize").map(|_| "usize".to_string()),
        tag("f32").map(|_| "f32".to_string()),
        tag("f64").map(|_| "f64".to_string()),
        tag("bool").map(|_| "bool".to_string()),
        tag("char").map(|_| "char".to_string()),
        parse_string_with_identity,
        tag("string").map(|_| "string".to_string()),
        tag("str").map(|_| "str".to_string()),
        tag("String").map(|_| "String".to_string()),
    ));
    
    // Try special types first, then built-in types, then type paths
    let (input, base) = alt((
        special_types_no_array,
        builtin_types,
        parse_type_path,
    )).parse(input)?;
    s += &base;
    Ok((input, s))
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

    // First try special types
    let special_types = alt((
        tag("_").map(|_| "_".to_string()),
        parse_tuple_type,
        parse_fn_type,
        parse_array_type,
        parse_pointer_type,
        preceded(ws(tag("dyn")), ws(parse_type_path)).map(|p| format!("dyn {}", p)),
        parse_simd_type,
        parse_lt_type,
    ));
    
    // Then try built-in types
    let builtin_types = alt((
        tag("i8").map(|_| "i8".to_string()),
        tag("i16").map(|_| "i16".to_string()),
        tag("i32").map(|_| "i32".to_string()),
        tag("i64").map(|_| "i64".to_string()),
        tag("u8").map(|_| "u8".to_string()),
        tag("u16").map(|_| "u16".to_string()),
        tag("u32").map(|_| "u32".to_string()),
        tag("u64").map(|_| "u64".to_string()),
        tag("usize").map(|_| "usize".to_string()),
        tag("f32").map(|_| "f32".to_string()),
        tag("f64").map(|_| "f64".to_string()),
        tag("bool").map(|_| "bool".to_string()),
        tag("char").map(|_| "char".to_string()),
        parse_string_with_identity,
        tag("string").map(|_| "string".to_string()),
        tag("str").map(|_| "str".to_string()),
        tag("String").map(|_| "String".to_string()),
    ));
    
    // Try special types first, then built-in types, then type paths
    let (input, base) = alt((
        special_types,
        builtin_types,
        parse_type_path,
    )).parse(input)?;
    s += &base;
    Ok((input, s))
}

pub fn parse_type_args(input: &str) -> IResult<&str, Vec<String>> {
    let (input, inner) = delimited(
        ws(tag("<")),
        parse_angle_bracketed_content_inner_slice,
        ws(tag(">")),
    )
    .parse(input)?;
    let (_, args) = terminated(
        separated_list0(ws(tag(",")), ws(parse_generic_arg_text)),
        opt(ws(tag(","))),
    )
    .parse(inner)?;
    Ok((input, args))
}

/// Parse generic argument text (now parses a full type)
pub fn parse_generic_arg_text(input: &str) -> IResult<&str, String> {
    parse_type(input)
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
pub fn parse_generic_param_as_string(input: &str) -> IResult<&str, String> {
    // Try to parse const parameter first: const N: usize
    let mut const_parser = map(
        (
            ws(tag("const")),
            ws(parse_ident),
            ws(tag(":")),
            ws(parse_type),
        ),
        |(_, name, _, ty)| format!("const {}: {}", name, ty),
    );
    
    // Try const parameter first
    if let Ok(result) = const_parser.parse(input) {
        return Ok(result);
    }
    
    // Fall back to type parameter
    let (input, param_name) = ws(parse_ident).parse(input)?;

    // Check for trait bounds
    let (input, bounds_str) = if let Ok((input, bounds)) = parse_trait_bounds(input) {
        (input, format!(": {}", bounds.join(" + ")))
    } else {
        (input, String::new())
    };

    Ok((input, format!("{}{}", param_name, bounds_str)))
}

/// Parse a single generic parameter as GenericParam enum
pub fn parse_generic_param_as_enum(input: &str) -> IResult<&str, GenericParam> {
    // Try to parse const parameter first: const N: usize
    let mut const_parser = map(
        (
            ws(tag("const")),
            ws(parse_ident),
            ws(tag(":")),
            ws(parse_type),
        ),
        |(_, name, _, ty)| GenericParam::Const { name, ty },
    );
    
    // Try const parameter first
    if let Ok(result) = const_parser.parse(input) {
        return Ok(result);
    }
    
    // Try to parse lifetime parameter
    if let Ok((input, lifetime)) = parse_lifetime_param(input) {
        return Ok((input, GenericParam::Lifetime { name: lifetime }));
    }
    
    // Fall back to type parameter
    let (input, param_name) = ws(parse_ident).parse(input)?;
    let mut bounds = Vec::new();
    
    // Check for trait bounds
    let input = if let Ok((input, parsed_bounds)) = parse_trait_bounds(input) {
        bounds = parsed_bounds;
        input
    } else {
        input
    };

    Ok((input, GenericParam::Type { name: param_name, bounds }))
}

/// Parse generic parameters including lifetime, type, and const parameters
/// Examples: "<'a, T>", "<'a, 'b, T: Display, U>", "<const N: usize, T>"
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
                    map(parse_generic_param_as_string, |p| (p, false)),
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

/// Parse generic parameters as GenericParam enum values
pub fn parse_generic_params_as_enum(input: &str) -> IResult<&str, Vec<GenericParam>> {
    // eprintln!("[DEBUG parse_generic_params_as_enum] input: {:?}", input); // Disabled for performance
    let (input, inner) = delimited(
        ws(tag("<")),
        parse_angle_bracketed_content_inner_slice,
        ws(tag(">")),
    )
    .parse(input)?;
    // eprintln!("[DEBUG parse_generic_params_as_enum] inner: {:?}", inner); // Disabled for performance
    let (_, params) = terminated(
        separated_list0(ws(tag(",")), ws(parse_generic_param_as_enum)),
        opt(ws(tag(","))),
    )
    .parse(inner)?;
    // eprintln!("[DEBUG parse_generic_params_as_enum] params: {:?}", params); // Disabled for performance

    Ok((input, params))
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

fn parse_angle_bracketed_content_inner_slice(input: &str) -> IResult<&str, &str> {
    let mut depth = 1;
    let mut chars = input.char_indices();
    
    while let Some((i, c)) = chars.next() {
        match c {
            '<' => depth += 1,
            '>' => {
                depth -= 1;
                if depth == 0 {
                    // We've matched the outer '>'; return content up to this point (excluding this '>')
                    return Ok((&input[i..], &input[0..i]));
                }
            }
            _ => {}
        }
    }
    
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::TakeUntil)))
}

/// Helper to parse angle-bracketed content, handling nested angle brackets
fn parse_angle_bracketed_content(input: &str) -> IResult<&str, String> {
    let mut depth = 0;
    let mut result = String::new();
    let mut chars = input.char_indices();
    
    while let Some((i, c)) = chars.next() {
        match c {
            '<' => {
                depth += 1;
                result.push(c);
            }
            '>' => {
                if depth == 0 {
                    return Ok((&input[i..], result));
                }
                depth -= 1;
                result.push(c);
            }
            _ => {
                result.push(c);
            }
        }
    }
    
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::TakeUntil)))
}

/// Helper to parse angle-bracketed content when the opening '<' has already been consumed.
/// Starts with depth=1 and returns the content inside the outermost brackets (excluding the outer brackets).
fn parse_angle_bracketed_content_inner(input: &str) -> IResult<&str, String> {
    let mut depth = 1;
    let mut result = String::new();
    let mut chars = input.char_indices();
    
    while let Some((i, c)) = chars.next() {
        match c {
            '<' => {
                depth += 1;
                result.push(c);
            }
            '>' => {
                depth -= 1;
                if depth == 0 {
                    // We've matched the outer '>'; return content up to this point (excluding this '>')
                    return Ok((&input[i..], result));
                }
                result.push(c);
            }
            _ => {
                result.push(c);
            }
        }
    }
    
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::TakeUntil)))
}

fn split_top_level_commas(input: &str) -> Vec<&str> {
    let mut result = Vec::new();
    let mut start = 0;
    let mut depth = 0;
    let chars = input.char_indices();
    for (i, c) in chars {
        match c {
            '<' => depth += 1,
            '>' => depth -= 1,
            ',' if depth == 0 => {
                result.push(&input[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    result.push(&input[start..]);
    result
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
