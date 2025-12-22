// src/frontend/parser/parser.rs
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0};
use nom::combinator::value;
use nom::multi::many1;
use nom::sequence::{pair, preceded};
use nom::{IResult, Parser};

#[allow(unused_imports)]
use nom::branch::alt;
#[allow(unused_imports)]
use nom::combinator::{map, opt};

pub fn ws<'a, F: FnMut(&'a str) -> IResult<&'a str, O>, O>(mut inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    move |input| {
        let (input, _) = multispace0(input)?;
        let (input, result) = inner(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, result))
    }
}

pub fn parse_ident(input: &str) -> IResult<&str, String> {
    map(pair(alpha1, alphanumeric0), |(first, rest): (&str, &str)| first.to_string() + rest)(input)
}

pub fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw)))(input)
}

pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    map(
        many1(preceded(opt(tag("::")), parse_ident)),
        |ids: Vec<String>| ids,
    )
    (input)
}
