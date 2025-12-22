// src/frontend/parser/parser.rs
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{alpha1, alphanumeric0, i64 as nom_i64, multispace0};
use nom::combinator::{map, opt, value};
use nom::sequence::{delimited, pair, preceded};
use nom::{IResult, Parser};

fn ws<'a, F, O>(mut inner: F) -> impl Parser<&'a str, O, nom::error::Error<&'a str>>
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

fn parse_ident(input: &str) -> IResult<&str, String> {
    map(pair(alpha1, alphanumeric0), |(first, rest): (&str, &str)| first.to_string() + rest)(input)
}

fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw)))(input)
}

fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    map(many1(preceded(opt(tag("::")), parse_ident)), |ids| ids)(input)
}
