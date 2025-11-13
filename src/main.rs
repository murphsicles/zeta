// src/main.rs
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::collections::HashMap;

// Token enum for Zeta syntax
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
    And,
    Plus,
    BraceOpen,
    BraceClose,
    AngleOpen,
    AngleClose,
    Hash,
    BracketOpen,
    BracketClose,
}

// Parser for tokens
fn identifier(input: &str) -> IResult<&str, Token> {
    let ident = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ));
    map(ident, |s: &str| Token::Ident(s.to_string()))(input)
}

fn keyword(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Concept, tag("concept")),
        value(Token::Impl, tag("impl")),
        value(Token::Fn, tag("fn")),
        value(Token::Where, tag("where")),
        value(Token::For, tag("for")),
        value(Token::Mut, tag("mut")),
        value(Token::And, tag("+")), // Simplified for Addable
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
        value(Token::Plus, char('+')),
        value(Token::BraceOpen, char('{')),
        value(Token::BraceClose, char('}')),
        value(Token::AngleOpen, tag("<")),
        value(Token::AngleClose, tag(">")),
        value(Token::Hash, tag("#")),
        value(Token::BracketOpen, tag("[") ),
        value(Token::BracketClose, tag("]")),
    ))(input)
}

pub fn tokenize(input: &str) -> IResult<&str, Vec<Token>> {
    many0(terminated(
        alt((identifier, keyword, symbol)),
        multispace0,
    ))(input)
}

// AST for Addable concept
#[derive(Debug, Clone)]
pub enum AstNode {
    ConceptDef {
        name: String,
        params: Vec<String>,
        methods: Vec<AstNode>,
    },
    Method {
        name: String,
        params: Vec<(String, String)>,
        ret: String,
    },
    ImplBlock {
        concept: String,
        ty: String,
        body: Vec<AstNode>,
    },
}

// Parser for concept decl
fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((
        tag("concept"),
        multispace0,
        map(identifier, |t| if let Token::Ident(n) = t { n } else { unreachable!() }),
        multispace0,
        opt(delimited(
            tag("<"),
            separated_list1(tag(","), identifier),
            tag(">"),
        )),
        multispace0,
        tag("{"),
        multispace0,
        many0(parse_method),
        tag("}"),
    ));

    let (i, (_, _, name, _, params_opt, _, _, _, methods, _)) = parser(input)?;
    let params = params_opt.unwrap_or_default().into_iter().filter_map(|t| if let Token::Ident(p) = t { Some(p) } else { None }).collect();
    Ok((i, AstNode::ConceptDef { name, params, methods }))
}

fn parse_method(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((
        tag("fn"),
        multispace0,
        map(identifier, |t| if let Token::Ident(n) = t { n } else { unreachable!() }),
        multispace0,
        delimited(
            tag("("),
            separated_list1(
                tag(","),
                separated_pair(
                    opt(tag("mut")),
                    tag(":"),
                    tuple((identifier, multispace0, tag(":"), multispace0, identifier)),
                ),
            ),
            tag(")"),
        ),
        multispace0,
        tag("->"),
        multispace0,
        identifier,
        multispace0,
        tag(";"),
    ));

    let (i, (_, _, name, _, params, _, _, _, ret, _, _)) = parser(input)?;
    let method_params: Vec<(String, String)> = params.into_iter().map(|(mut_opt, (_, _, _, _, ty))| {
        let param_name = if let Token::Ident(pn) = &mut_opt.1.0 { pn.clone() } else { "".to_string() }; // Simplified
        let ty_str = if let Token::Ident(t) = ty { t } else { "".to_string() };
        (param_name, ty_str)
    }).collect();
    let ret_str = if let Token::Ident(r) = ret { r } else { "".to_string() };
    Ok((i, AstNode::Method { name, params: method_params, ret: ret_str }))
}

// Main parser for Addable
pub fn parse_addable(input: &str) -> IResult<&str, AstNode> {
    parse_concept(input)
}

fn main() {
    let code = r#"
concept Addable<Rhs=Self> {
    fn add(self, rhs: Rhs) -> Self;
}
"#;
    match parse_addable(code) {
        Ok((_, ast)) => println!("{:?}", ast),
        Err(e) => eprintln!("Parse error: {:?}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        let input = "concept Addable < Rhs = Self { fn add ( self , rhs : Rhs ) -> Self ; }";
        let result = tokenize(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_addable() {
        let input = r#"
concept Addable<Rhs=Self> {
    fn add(self, rhs: Rhs) -> Self;
}
"#;
        let result = parse_addable(input);
        assert!(result.is_ok());
        if let AstNode::ConceptDef { name, params, methods } = result.unwrap().1 {
            assert_eq!(name, "Addable".to_string());
            assert_eq!(params, vec!["Rhs".to_string()]);
            assert_eq!(methods.len(), 1);
            if let AstNode::Method { name: mname, params: mparams, ret } = &methods[0] {
                assert_eq!(*mname, "add".to_string());
                assert_eq!(*ret, "Self".to_string());
                assert_eq!(mparams.len(), 2);
            }
        }
    }
  }
