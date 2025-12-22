// src/frontend/parser/parser.rs
//! Utility parsers for Zeta.
use nom::branch::alt;
use nom::bytes::complete::take_while1;
use nom::character::complete::{alpha1, alphanumeric0, multispace0};
use nom::combinator::{map, opt, value};
use nom::multi::many1;
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};

pub fn ws<'a, F, O>(mut inner: F) -> impl Parser<&'a str, O, nom::error::Error<&'a str>>
where
    F: Parser<&'a str, O, nom::error::Error<&'a str>>,
{
    move |input| {
        let (input, _) = multispace0(input)?;
        let (input, result) = inner.parse(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, result))
    }
}

pub fn parse_ident(input: &str) -> IResult<&str, String> {
    map(pair(alpha1, alphanumeric0), |(first, rest): (&str, &str)| first.to_string() + rest).parse(input)
}

fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw))).parse(input)
}

pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    map(
        many1(preceded(opt(tag("::")), parse_ident)),
        |ids: Vec<String>| ids,
    )
    .parse(input)
}
