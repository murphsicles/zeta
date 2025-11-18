use crate::ast::AstNode;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
};

fn identifier(input: &str) -> IResult<&str, String> {
    let (i, s) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)?;
    Ok((i, s.to_string()))
}

fn parse_int(input: &str) -> IResult<&str, i64> {
    map(digit1, |s: &str| s.parse().unwrap())(input)
}

fn parse_type(input: &str) -> IResult<&str, String> {
    let (i, name) = identifier(input)?;
    let (i, gens) = opt(delimited(
        tag("<"),
        separated_list1(tag(","), parse_type),
        tag(">"),
    ))(i)?;
    let gens = gens.map(|v| format!("<{}>", v.join(","))).unwrap_or_default();
    Ok((i, format!("{}{}", name, gens)))
}

fn parse_primary(input: &str) -> IResult<&str, AstNode> {
    alt((
        map(parse_int, AstNode::Lit),
        map(identifier, AstNode::Var),
        delimited(tag("("), parse_expr, tag(")")),
        map(parse_constructor, |(ty, args)| AstNode::Construct { ty, args }),
        map(parse_timing_owned, |(ty, inner)| AstNode::TimingOwned { ty, inner }),
    ))(input)
}

fn parse_constructor(input: &str) -> IResult<&str, (String, Vec<AstNode>)> {
    let (i, ty) = parse_type(input)?;
    let (i, _) = multispace0(i)?;
    let (i, args) = delimited(tag("["), separated_list0(tag(","), parse_expr), tag("]"))(i)?;
    Ok((i, (ty, args)))
}

fn parse_timing_owned(input: &str) -> IResult<&str, (String, AstNode)> {
    let (i, _) = tuple((tag("TimingOwned"), multispace0, tag("<"))(input)?;
    let (i, ty) = parse_type(i)?;
    let (i, _) = tuple((tag(">"), multispace0))(i)?;
    let (i, inner) = parse_expr(i)?;
    Ok((i, (ty, inner)))
}

fn parse_call_postfix(mut expr: AstNode, input: &str) -> IResult<&str, AstNode> {
    let (i, method) = preceded(tuple((multispace0, tag("."), multispace0)), identifier)(input)?;
    let (i, args) = opt(delimited(
        tag("("),
        separated_list0(tag(","), parse_expr),
        tag(")"),
    ))(i)?;
    let args = args.unwrap_or_default();
    Ok((i, AstNode::Call {
        receiver: Box::new(expr),
        method,
        args,
    }))
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    let (mut i, mut expr) = parse_primary(input)?;
    loop {
        match parse_call_postfix(expr.clone(), i) {
            Ok((ni, new_expr)) => {
                i = ni;
                expr = new_expr;
            }
            Err(_) => break,
        }
    }
    Ok((i, expr))
}

fn parse_let(input: &str) -> IResult<&str, AstNode> {
    let (i, _) = tuple((tag("let"), multispace0))(input)?;
    let (i, name) = identifier(i)?;
    let (i, ty) = opt(preceded(tuple((tag(":"), multispace0)), parse_type))(i)?;
    let (i, _) = tuple((multispace0, tag("="), multispace0))(i)?;
    let (i, rhs) = parse_expr(i)?;
    Ok((i, AstNode::Let {
        name,
        ty,
        rhs: Box::new(rhs),
    }))
}

fn parse_stmt(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_let,
        map(parse_expr, |e| AstNode::ExprStmt(Box::new(e))),
    ))(input)
}

pub fn parse_func_body(input: &str) -> IResult<&str, (Vec<AstNode>, Option<Box<AstNode>>)> {
    let (i, stmts) = many0(terminated(parse_stmt, tuple((multispace0, tag(";"), multispace0))))(input)?;
    let (i, ret_expr) = opt(parse_expr)(i)?;
    Ok((i, (stmts, ret_expr.map(Box::new))))
}

pub fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let generics_parser = opt(delimited(
        tag("<"),
        separated_list1(tag(","), tuple((identifier, opt(preceded(tag("="), identifier)))),
        tag(">"),
    ));

    let param_parser = tuple((
        opt(tag("mut")),
        tag(":"),
        identifier,
        multispace0,
        tag(":"),
        multispace0,
        parse_type,
    ));

    let (i, (_, attrs, _, _, name, _, generics, _, params, _, _, _, ret, _, where_clause, _, _, (body, ret_expr), _)) = tuple((
        multispace0,
        parse_attrs,
        tag("fn"),
        multispace0,
        identifier,
        multispace0,
        generics_parser,
        multispace0,
        delimited(tag("("), separated_list1(tag(","), param_parser), tag(")")),
        multispace0,
        tag("->"),
        multispace0,
        identifier,
        multispace0,
        opt(preceded(tag("where"), separated_list1(tag(","), tuple((identifier, tag(":"), identifier))))),
        multispace0,
        tag("{"),
        parse_func_body,
        tag("}"),
    ))(input)?;

    let params = params.into_iter().map(|(_, n, _, _, _, _, t)| (n, t)).collect();

    Ok((i, AstNode::FuncDef {
        name,
        generics: generics.unwrap_or_default().into_iter().map(|(n, _)| n).collect(),
        params,
        ret,
        body,
        ret_expr,
        where_clause,
        attrs,
    }))
}

 // other parsers unchanged (concept, impl, etc.)
 // stubs for parse_assign, parse_call, etc. removed - replaced by above
}
