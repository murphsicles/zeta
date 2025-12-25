// src/frontend/parser/parser.rs
use nom::IResult;
use nom::Parser;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric0, multispace0};
use nom::combinator::{opt, value};
use nom::error::ParseError;
use nom::multi::{many0, separated_list1};
use nom::sequence::{delimited, preceded};

pub fn ws<'a, P>(inner: P) -> impl Parser<&'a str, Output = P::Output, Error = P::Error>
where
    P: Parser<&'a str>,
    P::Error: ParseError<&'a str>,
{
    delimited(multispace0, inner, multispace0)
}

pub fn parse_ident(input: &str) -> IResult<&str, String> {
    let (input, (first, rest)) = nom::sequence::pair(alpha1, alphanumeric0).parse(input)?;
    Ok((input, format!("{}{}", first, rest)))
}

pub fn parse_keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input| value((), ws(tag(kw))).parse(input)
}

pub fn parse_path(input: &str) -> IResult<&str, Vec<String>> {
    many0(preceded(opt(tag("::")), parse_ident)).parse(input)
}

pub fn parse_generics(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        tag("<"),
        separated_list1(tag(","), ws(parse_ident)),
        tag(">"),
    )
    .parse(input)
}
