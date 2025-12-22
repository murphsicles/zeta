// src/frontend/parser/parser.rs
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0};
use nom::combinator::value;
use nom::multi::{many1, separated_list1};
use nom::sequence::{pair, preceded, delimited};
use nom::{IResult, Parser};

#[allow(unused_imports)]
use nom::branch::alt;
#[allow(unused_imports)]
use nom::combinator::{map, opt};

pub fn ws<'a, F, O>(inner: F) -> impl Parser<&'a str, O, nom::error::Error<&'a str>>
where
    F: Parser<&'a str, O, nom::error::Error<&'a str>>,
{
    move |input| {
        let (input, _) = multispace0.parse(input)?;
        let (input, result) = inner.parse(input)?;
        let (input, _) = multispace0.parse(input)?;
        Ok((input, result))
    }
}

pub fn parse_ident(input: &str) -> IResult<&str, String> {
    map(pair(alpha1, alphanumeric0), |(first, rest): (&str, &str)| first.to_string() + rest).parse(input)
}

pub fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw))).parse(input)
}

pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    map(
        many1(preceded(opt(tag("::")), parse_ident)),
        |ids: Vec<String>| ids,
    )
    .parse(input)
}

pub fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tag("<"), separated_list1(tag(","), parse_ident), tag(">")).parse(input)
}
