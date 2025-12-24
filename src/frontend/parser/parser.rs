// src/frontend/parser/parser.rs
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0};
use nom::combinator::value;
use nom::multi::{many1, separated_list1};
use nom::sequence::{pair, preceded, delimited};
use nom::{IResult};

#[allow(unused_imports)]
use nom::branch::alt;
#[allow(unused_imports)]
use nom::combinator::{map, opt};

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

pub fn parse_ident(input: &str) -> IResult<&str, String> {
    map(pair(alpha1, alphanumeric0), |(first, rest): (&str, &str)| first.to_string() + rest)(input)
}

pub fn parse_keyword(kw: &'static str) -> impl FnMut(&str) -> IResult<&str, ()> {
    value((), ws(tag(kw)))
}

pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    many1(preceded(opt(tag("::")), parse_ident))(input)
}

pub fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tag("<"), separated_list1(tag(","), parse_ident), tag(">"))(input)
}
