// src/parser.rs
use crate::ast::AstNode;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
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
) -> impl Fn(&str) -> IResult<&str, Option<String>> {
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
    let p = tuple((
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
    ));
    let (i, (_, _, traits, _, ty, _)) = p(input)?;
    Ok((
        i,
        AstNode::Derive {
            ty: ty.unwrap_or_default(),
            traits,
        },
    ))
}

pub fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let param_item = separated_pair(
        map_opt(identifier),
        opt(preceded(tag("="), map_opt(identifier))),
        tag(","),
    );
    let params_opt = opt(delimited(
        tag("<"),
        separated_list1(tag(","), param_item),
        tag(">"),
    ));
    let p = tuple((
        tag("concept"),
        multispace0,
        map_opt(identifier),
        multispace0,
        params_opt,
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_method),
        tag("}"),
    ));
    let (i, (_, _, name, _, params_opt, _, _, _, methods, _)) = p(input)?;
    let params: Vec<String> = params_opt
        .unwrap_or_default()
        .into_iter()
        .map(|(n, _): (Option<String>, Option<String>)| n.unwrap_or_default())
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
    let param_item = separated_pair(
        opt(tag("mut")),
        tag(":"),
        tuple((
            map_opt(identifier),
            multispace0,
            tag(":"),
            multispace0,
            map_opt(identifier),
        )),
    );
    let p = tuple((
        tag("fn"),
        multispace0,
        map_opt(identifier),
        multispace0,
        delimited(tag("("), separated_list1(tag(","), param_item), tag(")")),
        multispace0,
        tag("->"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag(";"),
    ));
    let (i, (_, _, name, _, params, _, _, _, ret, _, _)) = p(input)?;
    let method_params: Vec<(String, String)> = params
        .into_iter()
        .map(|(mut_opt, (pn, _, _, _, ty)): ((Option<()>, &str), (Option<String>, &str, &str, &str, Option<String>))| (pn.unwrap_or_default(), ty.unwrap_or_default()))
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

fn parse_struct(input: &str) -> IResult<&str, AstNode> {
    let field_item = separated_pair(map_opt(identifier), tag(":"), map_opt(identifier));
    let p = tuple((
        tag("struct"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag("{"),
        multispace0,
        separated_list1(tag(","), field_item),
        multispace0,
        tag("}"),
    ));
    let (i, (_, _, name, _, _, _, fields, _, _)) = p(input)?;
    let field_map: Vec<(String, String)> = fields
        .into_iter()
        .map(|(fname, ty): (Option<String>, Option<String>)| (fname.unwrap_or_default(), ty.unwrap_or_default()))
        .collect();
    Ok((
        i,
        AstNode::StructDef {
            name: name.unwrap_or_default(),
            fields: field_map,
        },
    ))
}

fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let p = tuple((
        tag("impl"),
        multispace0,
        map_opt(identifier),
        multispace0,
        opt(map_opt(identifier)),
        multispace0,
        tag("for"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_method),
        tag("}"),
    ));
    let (i, (_, _, concept, _, ty_opt, _, _, _, ty, _, _, _, methods, _)) = p(input)?;
    let ty_str = ty_opt.unwrap_or(ty).unwrap_or_default();
    Ok((
        i,
        AstNode::ImplBlock {
            concept: concept.unwrap_or_default(),
            ty: ty_str,
            body: methods,
        },
    ))
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    alt((
        map(int_literal, |t| if let Token::IntLit(n) = t { AstNode::Lit(n) } else { AstNode::Lit(0) }),
        map(identifier, |t| if let Token::Ident(s) = t { AstNode::Var(s) } else { AstNode::Var("".to_string()) }),
        parse_call,
        parse_borrow,
        parse_assign,
        parse_defer,
        parse_timing_owned,
    ))(input)
}

fn parse_call(input: &str) -> IResult<&str, AstNode> {
    let p = tuple((
        map_opt(identifier),
        tag("."),
        map_opt(identifier),
        opt(delimited(tag("("), separated_list1(tag(","), map_opt(identifier)), tag(")"))),
    ));
    let (i, (recv, _, method, args_opt)) = p(input)?;
    Ok((
        i,
        AstNode::Call {
            receiver: recv.unwrap_or_default(),
            method: method.unwrap_or_default(),
            args: args_opt.unwrap_or_default(),
        },
    ))
}

fn parse_borrow(input: &str) -> IResult<&str, AstNode> {
    preceded(tag("&"), map_opt(identifier))
        .map(|v: Option<String>| AstNode::Borrow(v.unwrap_or_default()))(input)
}

fn parse_assign(input: &str) -> IResult<&str, AstNode> {
    separated_pair(map_opt(identifier), tag("="), parse_expr)
        .map(|(var, _, expr): (Option<String>, &str, AstNode)| {
            AstNode::Assign(var.unwrap_or_default(), Box::new(expr))
        })(input)
}

fn parse_defer(input: &str) -> IResult<&str, AstNode> {
    preceded(tag("defer"), delimited(tag("{"), parse_expr, tag("}")))
        .map(|expr| AstNode::Defer(Box::new(expr)))(input)
}

fn parse_timing_owned(input: &str) -> IResult<&str, AstNode> {
    let p = tuple((
        tag("TimingOwned"),
        tag("<"),
        map_opt(identifier),
        tag(">"),
        tag("("),
        parse_expr,
        tag(")"),
    ));
    let (i, (_, _, ty, _, _, expr, _)) = p(input)?;
    Ok((
        i,
        AstNode::TimingOwned {
            ty: ty.unwrap_or_default(),
            inner: Box::new(expr),
        },
    ))
}

fn parse_actor(input: &str) -> IResult<&str, AstNode> {
    let p = tuple((
        tag("actor"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag("{"),
        multispace0,
        many0(alt((parse_async_method, parse_method))),
        tag("}"),
    ));
    let (i, (_, _, name, _, _, _, methods, _)) = p(input)?;
    Ok((
        i,
        AstNode::ActorDef {
            name: name.unwrap_or_default(),
            methods,
        },
    ))
}

fn parse_async_method(input: &str) -> IResult<&str, AstNode> {
    let param_item = parse_param;
    let p = tuple((
        tag("async"),
        multispace0,
        tag("fn"),
        multispace0,
        map_opt(identifier),
        multispace0,
        delimited(tag("("), separated_list1(tag(","), param_item), tag(")")),
        multispace0,
        tag("->"),
        multispace0,
        map_opt(identifier),
        multispace0,
        tag(";"),
    ));
    let (i, (_, _, _, _, name, _, params, _, _, _, ret, _, _)) = p(input)?;
    let method_params: Vec<(String, String)> = params
        .into_iter()
        .map(|(pn, ty): (Option<String>, Option<String>)| (pn.unwrap_or_default(), ty.unwrap_or_default()))
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
    let p = tuple((
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
    ));
    let (i, (_, _, ty, _, args_opt, _, _)) = p(input)?;
    Ok((
        i,
        AstNode::SpawnActor {
            actor_ty: ty.unwrap_or_default(),
            init_args: args_opt.unwrap_or_default(),
        },
    ))
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let generics_item = separated_pair(
        map_opt(identifier),
        opt(preceded(tag("="), map_opt(identifier))),
        tag(","),
    );
    let generics_opt = opt(delimited(
        tag("<"),
        separated_list1(tag(","), generics_item),
        tag(">"),
    ));
    let param_item = separated_pair(
        opt(tag("mut")),
        tag(":"),
        tuple((
            map_opt(identifier),
            multispace0,
            tag(":"),
            multispace0,
            map_opt(identifier),
        )),
    );
    let where_opt = opt(parse_where_clause);
    let p = tuple((
        multispace0,
        parse_attrs,
        tag("fn"),
        multispace0,
        map_opt(identifier),
        multispace0,
        generics_opt,
        multispace0,
        delimited(tag("("), separated_list1(tag(","), param_item), tag(")")),
        multispace0,
        tag("->"),
        multispace0,
        map_opt(identifier),
        multispace0,
        where_opt,
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_expr),
        tag("}"),
    ));
    let (i, (_, attrs_opt, _, _, name, _, generics_opt, _, params, _, _, _, ret, _, where_opt, _, _, _, body, _)) = p(input)?;
    let attrs: Vec<String> = attrs_opt.unwrap_or_default();
    let generics: Vec<String> = generics_opt
        .unwrap_or_default()
        .into_iter()
        .map(|(n, _): (Option<String>, Option<String>)| n.unwrap_or_default())
        .collect();
    let func_params: Vec<(String, String)> = params
        .into_iter()
        .map(|(mut_opt, (pn, _, _, _, ty)): ((Option<()>, &str), (Option<String>, &str, &str, &str, Option<String>))| (pn.unwrap_or_default(), ty.unwrap_or_default()))
        .collect();
    Ok((
        i,
        AstNode::FuncDef {
            name: name.unwrap_or_default(),
            generics,
            params: func_params,
            ret: ret.unwrap_or_default(),
            body,
            where_clause: where_opt,
            attrs,
        },
    ))
}

fn parse_where_clause(input: &str) -> IResult<&str, Vec<(String, String)>> {
    preceded(
        tag("where"),
        separated_list1(
            tag(","),
            separated_pair(map_opt(identifier), tag(":"), map_opt(identifier)),
        ),
    )(input)
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
