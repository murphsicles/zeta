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

pub fn ws<'a, P: Parser<&'a str>>(inner: P) -> impl Parser<&'a str> {
    delimited(multispace0, inner, multispace0)
}

pub fn parse_ident(input: &str) -> IResult<&str, String> {
    map(pair(alpha1, alphanumeric0), |(first, rest): (&str, &str)| first.to_string() + rest).parse(input)
}

pub fn parse_keyword(kw: &'static str) -> impl Parser<&str, Output = (), Error = nom::error::Error<&str>> {
    value((), ws(tag::<&str, &str, nom::error::Error<&str>>(kw)))
}

pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    many1(preceded(opt(tag("::")), parse_ident)).parse(input)
}

pub fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    delimited(tag("<"), separated_list1(tag(","), parse_ident), tag(">")).parse(input)
}
