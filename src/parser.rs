// src/parser.rs
use crate::ast::AstNode;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Concept,
    Impl,
    Fn,
    Ident(String),
    Colon,
    Arrow,
    LParen,
    RParen,
    Comma,
    Eq,
    Semi,
    Lt,
    Gt,
    Where,
    For,
    Mut,
    BraceOpen,
    BraceClose,
    IntLit(i64),
    AiOpt,
    Send,
    Sync,
    Derive,
    CacheSafe,
}

fn identifier(input: &str) -> IResult<&str, Token> {
    let ident = recognize((
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ));
    map(ident, |s: &str| Token::Ident(s.to_string()))(input)
}

fn int_literal(input: &str) -> IResult<&str, Token> {
    map(digit1, |s: &str| Token::IntLit(s.parse().unwrap()))(input)
}

fn keyword(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Concept, tag("concept")),
        value(Token::Impl, tag("impl")),
        value(Token::Fn, tag("fn")),
        value(Token::Where, tag("where")),
        value(Token::For, tag("for")),
        value(Token::Mut, tag("mut")),
        value(Token::AiOpt, tag("ai_opt")),
        value(Token::Send, tag("Send")),
        value(Token::Sync, tag("Sync")),
        value(Token::Derive, tag("derive")),
        value(Token::CacheSafe, tag("CacheSafe")),
    ))(input)
}

fn symbol(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Colon, char(':')),
        value(Token::Arrow, tag("->")),
        value(Token::LParen, char('(')),
        value(Token::RParen, char(')')),
        value(Token::Comma, char(',')),
        value(Token::Eq, char('=')),
        value(Token::Semi, char(';')),
        value(Token::Lt, char('<')),
        value(Token::Gt, char('>')),
        value(Token::BraceOpen, char('{')),
        value(Token::BraceClose, char('}')),
    ))(input)
}

pub fn tokenize(input: &str) -> IResult<&str, Vec<Token>> {
    many0(terminated(
        alt((int_literal, identifier, keyword, symbol)),
        multispace0,
    ))(input)
}

fn map_opt(
    p: impl Fn(&str) -> IResult<&str, Token>,
) -> impl Fn(&str) -> IResult<&str, Option<String>> + '_ {
    move |input| {
        let (i, t) = p(input)?;
        match t {
            Token::Ident(s) => Ok((i, Some(s))),
            _ => Ok((i, None)),
        }
    }
}

fn parse_attrs(input: &str) -> IResult<&str, Vec<String>> {
    many0(preceded(
        tag("#["),
        terminated(map_opt(identifier), tag("]")),
    ))(input)
}

fn parse_derive(input: &str) -> IResult<&str, AstNode> {
    let parser = (
        tag("derive"),
        multispace0,
        delimited(
            tag("("),
            separated_list1(tag(","), map_opt(identifier)),
            tag(")"),
        ),
        multispace0,
        map_opt(identifier),
        tag(";"),
    );
    let (i, (_, _, traits, _, ty, _)) = parser(input)?;
    Ok((
        i,
        AstNode::Derive {
            ty: ty.unwrap_or_default(),
            traits,
        },
    ))
}

pub fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let parser = (
        tag("concept"),
        multispace0,
        map_opt(identifier),
        multispace0,
        opt(delimited(
            tag("<"),
            separated_list1(
                tag(","),
                separated_pair(
                    map_opt(identifier),
                    opt(preceded(tag("="), map_opt(identifier))),
                    tag(","),
                ),
            ),
            tag(">"),
        )),
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_method),
        tag("}"),
    );
    let (i, (_, _, name, _, params_opt, _, _, _, methods, _)) = parser(input)?;
    let params: Vec<String> = params_opt
        .unwrap_or_default()
        .into_iter()
        .map(|(n, _)| n.unwrap_or_default())
        .collect();
    Ok((
        i,
        AstNode::ConceptDef {
            name: name.unwrap_or_default(),
            params,
            methods,
        },
    ))
}

fn parse_method(input: &str) -> IResult<&str, AstNode> {
    let param_parser = separated_pair(
        opt(tag("mut")),
        tag(":"),
        (
            map_opt(identifier),
            multispace0,
            tag(":"),
            multispace0,
            map_opt(identifier),
        ),
    );
    let parser = (
        tag("fn"),
        multispace0,
        map_opt(identifier),
        multispace0,
        delimited(tag("("), separated_list1(tag(","), param_parser), tag(")")),
        multispace0,
        tag("->"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag(";"),
    );
    let (i, (_, _, name, _, params, _, _, _, ret, _, _)) = parser(input)?;
    let method_params: Vec<(String, String)> = params
        .into_iter()
        .map(|(mut_opt, (pn, _, _, _, ty))| (pn.unwrap_or_default(), ty.unwrap_or_default()))
        .collect();
    Ok((
        i,
        AstNode::Method {
            name: name.unwrap_or_default(),
            params: method_params,
            ret: ret.unwrap_or_default(),
        },
    ))
}

fn parse_param(input: &str) -> IResult<&str, (String, String)> {
    separated_pair(map_opt(identifier), tag(":"), map_opt(identifier))(input)
}

fn parse_spawn_actor(input: &str) -> IResult<&str, AstNode> {
    let parser = (
        tag("spawn_actor"),
        multispace0,
        map_opt(identifier),
        multispace0,
        delimited(
            tag("("),
            separated_list1(tag(","), map_opt(identifier)),
            tag(")"),
        ),
        multispace0,
        tag(";"),
    );
    let (i, (_, _, ty, _, args_opt, _, _)) = parser(input)?;
    Ok((
        i,
        AstNode::SpawnActor {
            actor_ty: ty.unwrap_or_default(),
            init_args: args_opt.unwrap_or_default(),
        },
    ))
}

fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let parser = (
        tag("impl"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag("for"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_method),
        tag("}"),
    );
    let (i, (_, _, concept, _, _, _, ty, _, _, _, body, _)) = parser(input)?;
    Ok((
        i,
        AstNode::ImplBlock {
            concept: concept.unwrap_or_default(),
            ty: ty.unwrap_or_default(),
            body,
        },
    ))
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let generics_parser = opt(delimited(
        tag("<"),
        separated_list1(
            tag(","),
            separated_pair(
                map_opt(identifier),
                opt(preceded(tag("="), map_opt(identifier))),
                tag(","),
            ),
        ),
        tag(">"),
    ));
    let param_parser = separated_pair(
        opt(tag("mut")),
        tag(":"),
        (
            map_opt(identifier),
            multispace0,
            tag(":"),
            multispace0,
            map_opt(identifier),
        ),
    );
    let where_parser = opt(preceded(
        tag("where"),
        separated_list1(
            tag(","),
            separated_pair(map_opt(identifier), tag(":"), map_opt(identifier)),
        ),
    ));
    let parser = (
        multispace0,
        parse_attrs,
        tag("fn"),
        multispace0,
        map_opt(identifier),
        multispace0,
        generics_parser,
        multispace0,
        delimited(tag("("), separated_list1(tag(","), param_parser), tag(")")),
        multispace0,
        tag("->"),
        multispace0,
        map_opt(identifier),
        multispace0,
        where_parser,
        multispace0,
        tag("{"),
        multispace0,
        many0(alt((
            parse_assign,
            parse_call,
            parse_borrow,
            parse_defer,
            parse_timing_owned,
            parse_spawn_actor,
        ))),
        tag("}"),
    );
    let (
        i,
        (
            _,
            attrs_opt,
            _,
            _,
            name,
            _,
            generics_opt,
            _,
            params,
            _,
            _,
            _,
            ret,
            _,
            where_opt,
            _,
            _,
            _,
            body,
            _,
        ),
    ) = parser(input)?;
    let attrs: Vec<String> = attrs_opt.unwrap_or_default();
    let generics: Vec<String> = generics_opt
        .unwrap_or_default()
        .into_iter()
        .map(|(n, _): (Option<String>, Option<String>)| n.unwrap_or_default())
        .collect();
    let func_params: Vec<(String, String)> = params
        .into_iter()
        .map(|(mut_opt, (pn, _, _, _, ty)): (Option<()>, (Option<String>, (), (), (), Option<String>))| (pn.unwrap_or_default(), ty.unwrap_or_default()))
        .collect();
    let where_clause = where_opt.unwrap_or_default();
    Ok((
        i,
        AstNode::FuncDef {
            name: name.unwrap_or_default(),
            generics,
            params: func_params,
            ret: ret.unwrap_or_default(),
            body,
            where_clause: Some(where_clause),
            attrs,
        },
    ))
}

fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    // Stub implementation for assign
    Ok((input, AstNode::Assign("".to_string(), Box::new(AstNode::Lit(0)))))
}

fn parse_call(input: &str) -> IResult<&str, AstNode> {
    // Stub implementation for call
    Ok((input, AstNode::Call {
        method: "".to_string(),
        receiver: "".to_string(),
        args: vec![],
    }))
}

fn parse_borrow(input: &str) -> IResult<&str, AstNode> {
    // Stub implementation for borrow
    Ok((input, AstNode::Borrow("".to_string())))
}

fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    // Stub implementation for defer
    Ok((input, AstNode::Defer(Box::new(AstNode::Lit(0)))))
}

fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    // Stub implementation for timing_owned
    Ok((input, AstNode::TimingOwned {
        ty: "".to_string(),
        inner: Box::new(AstNode::Lit(0)),
    }))
}

fn parse_actor(input: &str) -> IResult<&str, AstNode> {
    // Stub implementation for actor
    Ok((input, AstNode::ActorDef {
        name: "".to_string(),
        methods: vec![],
    }))
}

fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    // Stub implementation for struct
    Ok((input, AstNode::StructDef {
        name: "".to_string(),
        fields: vec![],
    }))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(alt((
        parse_func,
        parse_actor,
        parse_impl,
        parse_concept,
        parse_derive,
        parse_struct,
    )))(input)
}
