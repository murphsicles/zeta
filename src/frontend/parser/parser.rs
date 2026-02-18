// src/frontend/parser/parser.rs
//! Top-level parser and common combinators for the Zeta language.
use nom::IResult;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while};
use nom::character::complete::{alpha1, multispace1, satisfy};
use nom::combinator::{opt, recognize, value, verify};
use nom::multi::{many0, separated_list0, separated_list1};
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

pub fn ws<'a, P>(
    inner: P,
) -> impl Parser<&'a str, Output = P::Output, Error = nom::error::Error<&'a str>>
where
    P: Parser<&'a str, Error = nom::error::Error<&'a str>>,
{
    delimited(skip_ws_and_comments, inner, skip_ws_and_comments)
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
                "dyn", "box", "as", "true",
                "false",
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

pub fn parse_type(input: &str) -> IResult<&str, String> {
    let (mut input, mut s) = (input, String::new());
    loop {
        if let Ok((i, _)) = ws(tag("&mut")).parse(input) {
            s.push_str("&mut ");
            input = i;
        } else if let Ok((i, _)) = ws(tag("&")).parse(input) {
            s.push_str("&");
            input = i;
        } else {
            break;
        }
    }
    let (input, base) = alt((
        tag("_").map(|_| "_".to_string()),
        parse_tuple_type,
        parse_fn_type,
        preceded(ws(tag("dyn")), ws(parse_type_path)).map(|p| format!("dyn {}", p)),
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

pub fn parse_generic_params(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        ws(tag("<")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_ident)),
            opt(ws(tag(","))),
        ),
        ws(tag(">")),
    )
    .parse(input)
}
