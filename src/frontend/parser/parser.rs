// src/frontend/parser/parser.rs
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0};
use nom::combinator::{opt, value};
use nom::multi::{many0, separated_list1};
use nom::sequence::delimited;
use nom::{IResult, Parser};
use nom::error::ParseError;

/// Wraps a parser with whitespace handling (both leading and trailing)
pub fn ws<'a, P, O, E>(inner: P) -> impl Parser<&'a str, O, E>
where
    P: Parser<&'a str, O, E>,
    E: ParseError<&'a str>,
{
    delimited(multispace0, inner, multispace0)
}

/// Parses an identifier (alphabetic start, followed by alphanumeric)
pub fn parse_ident(input: &str) -> IResult<&str, String> {
    let (input, (first, rest)) = nom::sequence::pair(alpha1, alphanumeric0).parse(input)?;
    Ok((input, format!("{}{}", first, rest)))
}

/// Parses a keyword and consumes surrounding whitespace
pub fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw))).parse(input)
}

/// Parses a path like `foo::bar::baz` or just `foo`
pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    many0(preceded(opt(tag("::")), parse_ident)).parse(input)
}

/// Parses generic type arguments like `<T, U>`
pub fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tag("<"), separated_list1(tag(","), ws(parse_ident)), tag(">")).parse(input)
}
