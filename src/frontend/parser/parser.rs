// src/frontend/parser/parser.rs
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0};
use nom::combinator::{opt, value};
use nom::multi::{many1, separated_list1};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};

/// Wraps a parser with whitespace handling (both leading and trailing)
pub fn ws<'a, F, O>(mut inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    move |input| {
        let (input, _) = multispace0(input)?;
        let (input, result) = inner(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, result))
    }
}

/// Parses an identifier (alphabetic start, followed by alphanumeric)
pub fn parse_ident(input: &str) -> IResult<&str, String> {
    let (input, (first, rest)) = pair(alpha1, alphanumeric0).parse(input)?;
    Ok((input, format!("{}{}", first, rest)))
}

/// Parses a keyword and consumes surrounding whitespace
pub fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw))).parse(input)
}

/// Parses a path like `foo::bar::baz` or just `foo`
pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    many1(preceded(opt(tag("::")), parse_ident)).parse(input)
}

/// Parses generic type arguments like `<T, U>`
pub fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tag("<"), separated_list1(tag(","), ws(parse_ident)), tag(">")).parse(input)
}
