// src/parser.rs
//! Nom-based parser for Zeta syntax.
//! Supports function definitions, calls, literals, variables, assigns, TimingOwned, defer, concepts, impls, and spawn.
//! Extended for self-host: concepts { methods }, impls for types, enums, structs, tokens.
//! Now with generics: fn name<T,U>(params) -> Ret { body }
//! Added hybrid traits: structural dispatch via method? (e.g., obj.add?(b) for ad-hoc structural lookup)
//! Added partial specialization: method::<T,U>(args) for type args in calls.
//! Added: Field access as postfix .field (no args).
//! Added: Basic match expr: match e { pat => expr, ... }.
//! Added: Generics in impl: impl<T> Concept for Type<T> { ... }.
//! Added: String literal parsing enhanced for unified strings (basic f-string stub as concat).
//! Added: Full f-strings: f"hello {expr} world" parsed as concat calls.
//! Added: Error recovery: use nom's .recover() or alt with empty on error.
//! Added: Doc comments: /// text before defs, attached to AstNode.
use crate::ast::{AstNode, Pattern};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1, escaped, take_until};
use nom::character::complete::{
    alpha1, alphanumeric0, char as nom_char, i64 as nom_i64, multispace0, one_of,
};
use nom::combinator::{map, opt, value, recognize, success, all_consuming};
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, preceded, tuple, pair};
use nom::{IResult, Parser, Err, error::{ParseError, ErrorKind}};
#[allow(dead_code)]
type FnParse = (
    (),
    String,
    Vec<String>,
    Vec<(String, String)>,
    Option<String>,
    Vec<AstNode>,
);
/// Whitespace wrapper for parsers.
fn ws<'a, O>(
    inner: impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>> {
    delimited(multispace0, inner, multispace0)
}
/// Parses doc comment: /// text.
fn parse_doc_comment<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    preceded(tag("///"), take_until("\n")).map(|(docs, _)| AstNode::DocComment(docs.to_string()))
}
/// Parses an identifier (alpha + alphanum).
fn parse_ident<'a>() -> impl Parser<&'a str, Output = String, Error = nom::error::Error<&'a str>> {
    map(alpha1.and(alphanumeric0), |(first, rest): (&str, &str)| {
        first.to_string() + rest
    })
}
/// Parses keyword.
#[allow(dead_code)]
fn parse_keyword<'a>(
    kw: &'static str,
) -> impl Parser<&'a str, Output = (), Error = nom::error::Error<&'a str>> {
    value((), ws(tag(kw)))
}
/// Parses an integer literal.
fn parse_literal<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    map(nom_i64, AstNode::Lit)
}
/// Parses interpolated part in f-string: text or {expr}.
fn parse_fstring_part<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((
        // Text part
        map(take_while1(|c| c != '{' && c != '"' && c != '}'), |s: &str| AstNode::StringLit(s.to_string())),
        // {expr}
        preceded(tag("{"), delimited(tag("{"), parse_expr_recover(), tag("}"))).map(|expr| expr),
    ))
}
/// Parses f-string: f" parts ".
fn parse_fstring<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    preceded(tag("f\""),
        map(separated_list1(success(()), parse_fstring_part()), |parts| {
            // Fold parts into concat calls
            if parts.is_empty() {
                AstNode::StringLit("".to_string())
            } else if parts.len() == 1 {
                parts[0].clone()
            } else {
                // Build chained concat: ((part0.concat part1).concat part2)...
                let mut current = parts[0].clone();
                for part in parts[1..].iter() {
                    current = AstNode::Call {
                        receiver: Some(Box::new(current)),
                        method: "concat".to_string(),
                        args: vec![part.clone()],
                        type_args: vec![],
                        structural: false,
                    };
                }
                current
            }
        })
    ).and(tag("\"")).map(|(node, _)| node)
}
/// Parses string literal, now with f-string.
fn parse_string_lit<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((parse_fstring(), // f"..."
        // Regular "..."
        delimited(
            tag("\""),
            escaped(
                take_while1(|c| c != '"' && c != '\\'),
                one_of(r#"\"nrt"#),
                |c| match c {
                    '"' => Ok(("".into(), "\"".into())),
                    _ => unreachable!(),
                },
            ),
            tag("\""),
        ).map(|s| AstNode::StringLit(s.to_string()))
    ))
}
/// Parses a variable reference.
fn parse_variable<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    map(parse_ident(), AstNode::Var)
}
/// Parses path: A::B.
#[allow(dead_code)]
fn parse_path<'a>() -> impl Parser<&'a str, Output = Vec<String>, Error = nom::error::Error<&'a str>>
{
    map(
        many1(preceded(opt(tag("::")), parse_ident())),
        |ids: Vec<String>| ids,
    )
}
/// Parses atom: lit | var | str.
fn parse_atom<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((parse_literal(), parse_string_lit(), parse_variable()))
}
/// Parses TimingOwned<ty>(atom).
fn parse_timing_owned<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(tag("TimingOwned"))
        .and(delimited(tag("<"), parse_ident(), tag(">")))
        .and(tag("("))
        .and(parse_atom())
        .and(tag(")"))
        .map(|((((_, ty), _), inner), _)| AstNode::TimingOwned {
            ty,
            inner: Box::new(inner),
        })
}
/// Parses primary: atom | TimingOwned | (expr).
fn parse_primary<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((
        parse_atom(),
        parse_timing_owned(),
        delimited(tag("("), parse_postfix_recover(), tag(")")), // Recursive postfix
    ))
}
/// Error recovery wrapper: try parser, fallback to empty/partial.
fn recover<F, O, E: ParseError<&'a str>>(
    mut inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    move |i| {
        match inner(i) {
            Ok(res) => Ok(res),
            Err(nom::Err::Error(e)) => {
                // Recover by skipping to next ws or ;
                let skipped = take_while1(|c| c != ';' && !c.is_whitespace())(i);
                match skipped {
                    Ok((rest, _)) => Ok((rest, AstNode::Var("recover".to_string()))), // Stub node
                    Err(e) => Err(e),
                }
            }
            Err(e) => Err(e),
        }
    }
}
/// Parses postfix: primary .field.
fn parse_postfix<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let field_access = parse_primary()
        .and(ws(tag(".")))
        .and(parse_ident())
        .map(|((receiver, _), field)| AstNode::FieldAccess {
            receiver: Box::new(receiver),
            field,
        });
    alt((field_access, parse_primary()))
}
/// Parses pattern for match arms (basic lit | var).
fn parse_pattern<'a>() -> impl Parser<&'a str, Output = Pattern, Error = nom::error::Error<&'a str>> {
    alt((
        map(nom_i64, Pattern::Lit),
        map(parse_ident(), Pattern::Var),
        // Variant: Variant(subpat)
        preceded(tag("Variant("), delimited(tag("("), parse_pattern(), tag(")"))).map(|pat| Pattern::Variant("Variant".to_string(), vec![pat])),
    ))
}
/// Parses match arm: pat => expr.
fn parse_arm<'a>() -> impl Parser<&'a str, Output = (Pattern, AstNode), Error = nom::error::Error<&'a str>> {
    parse_pattern()
        .and(ws(tag("=>")))
        .and(parse_postfix())
        .map(|((pat, _), expr)| (pat, expr))
}
/// Parses match: match expr { arms }.
fn parse_match<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    ws(tag("match"))
        .and(parse_postfix())
        .and(ws(delimited(tag("{"), separated_list1(tag(","), parse_arm()), tag("}"))))
        .map(|((_, expr), arms)| AstNode::Match {
            expr: Box::new(expr),
            arms: arms.into_iter().map(|(p, e)| (p, Box::new(e))).collect(),
        })
}
/// Parses base expression: postfix | match.
fn parse_base_expr<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((parse_postfix(), parse_match()))
}
/// Parses structural marker: ? after method name.
fn parse_structural<'a>() -> impl Parser<&'a str, Output = bool, Error = nom::error::Error<&'a str>>
{
    map(opt(nom_char('?')), |opt_q| opt_q.is_some())
}
/// Parses type args: <T,U>.
fn parse_type_args<'a>()
-> impl Parser<&'a str, Output = Vec<String>, Error = nom::error::Error<&'a str>> {
    delimited(tag("<"), separated_list1(tag(","), parse_ident()), tag(">"))
}
/// Parses type: ident or ident<args>.
fn parse_type<'a>() -> impl Parser<&'a str, Output = String, Error = nom::error::Error<&'a str>> {
    let simple = parse_ident();
    let generic = parse_ident()
        .and(opt(parse_type_args()))
        .map(|(id, args_opt)| {
            let mut s = id.clone();
            if let Some(args) = args_opt {
                s.push_str(&format!("<{}>", args.join(",")));
            }
            s
        });
    alt((simple, generic))
}
/// Parses generics: <T,U>.
fn parse_generics<'a>() -> impl Parser<&'a str, Output = Vec<String>, Error = nom::error::Error<&'a str>> {
    delimited(tag("<"), separated_list1(tag(","), parse_ident()), tag(">"))
}
/// Parses call: base.method?(<T,U>)(args).
fn parse_call<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let base = parse_base_expr()
        .and(ws(tag(".")))
        .and(parse_ident())
        .and(parse_structural())
        .and(opt(ws(parse_type_args())))
        .and(delimited(tag("("), separated_list1(tag(","), parse_postfix()), tag(")")));
    base.map(|(((((base, _), method), structural), type_args_opt), args)| AstNode::Call {
        receiver: Some(Box::new(base)),
        method,
        args,
        type_args: type_args_opt.unwrap_or(vec![]),
        structural,
    })
}
/// Parses assign: ident = expr.
fn parse_assign<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    parse_ident()
        .and(ws(tag("=")))
        .and(parse_base_expr())
        .map(|((name, _), expr)| AstNode::Assign(name, Box::new(expr)))
}
/// Parses stmt: assign | call | defer | spawn.
fn parse_stmt<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    alt((
        parse_assign(),
        parse_call(),
        preceded(ws(tag("defer")), map(parse_call(), |c| AstNode::Defer(Box::new(c)))),
        preceded(
            ws(tag("spawn")),
            tuple((parse_ident(), delimited(tag("("), many0(parse_base_expr()), tag(")"))))
                .map(|(func, args)| AstNode::Spawn { func, args }),
        ),
    ))
}
/// Recursive expr parser with recovery.
fn parse_expr_recover<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    recover(parse_base_expr)
}
/// Parses expr: base for now.
fn parse_expr<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    parse_base_expr()
}
/// Parses function: optional docs, fn name<gens>(params: types) -> ret { body }.
fn parse_func<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let docs_opt = opt(ws(parse_doc_comment()));
    let parse_fn_kw = value((), ws(tag("fn")));
    let parse_name = ws(parse_ident());
    let parse_generics_opt = opt(ws(parse_generics()));
    let parse_param_pair = parse_ident()
        .and(ws(tag(":")))
        .and(ws(parse_type()))
        .map(|((n, _), t): ((String, &'a str), String)| (n, t));
    let parse_params = delimited(tag("("), many0(parse_param_pair), tag(")"));
    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_type())));
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_stmt())), ws(tag("}")));
    docs_opt
        .and(parse_fn_kw)
        .and(parse_name)
        .and(parse_generics_opt)
        .and(parse_params)
        .and(parse_ret)
        .and(parse_body)
        .map(
            |((((((docs_opt, _fn_kw), name), generics_opt), params), ret_opt), body)| {
                let generics = generics_opt.unwrap_or(vec![]);
                let ret = ret_opt.unwrap_or_else(|| "i64".to_string());
                let docs = if let Some(AstNode::DocComment(s)) = docs_opt {
                    Some(s)
                } else {
                    None
                };
                AstNode::FuncDef {
                    name,
                    generics,
                    params,
                    ret,
                    body,
                    attrs: vec![],
                    ret_expr: None,
                    docs,
                }
            },
        )
}
/// Parses method sig for concept/impl: optional docs, name<gens>(params) -> ret, with optional generics.
fn parse_method_sig<'a>()
-> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let docs_opt = opt(ws(parse_doc_comment()));
    let parse_name = parse_ident();
    let parse_generics_opt = opt(ws(parse_generics()));
    let parse_params = delimited(tag("("), many0(parse_ident()), tag(")")); // Types as i64
    let parse_ret = opt(preceded(ws(tag("->")), ws(parse_type())));
    docs_opt
        .and(parse_name)
        .and(parse_generics_opt)
        .and(parse_params)
        .and(parse_ret)
        .map(
            |((((docs_opt, name), generics_opt), params), ret_opt)| {
                let docs = if let Some(AstNode::DocComment(s)) = docs_opt {
                    Some(s)
                } else {
                    None
                };
                AstNode::Method {
                    name,
                    params: params
                        .into_iter()
                        .map(|p| (p.clone(), "i64".to_string()))
                        .collect(),
                    ret: ret_opt.unwrap_or_else(|| "i64".to_string()),
                    generics: generics_opt.unwrap_or(vec![]),
                    docs,
                }
            },
        )
}
/// Parses concept: optional docs, concept name { methods }.
fn parse_concept<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    let docs_opt = opt(ws(parse_doc_comment()));
    let parse_kw = value((), ws(tag("concept")));
    let parse_name = ws(parse_ident());
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig())), ws(tag("}")));
    docs_opt
        .and(parse_kw)
        .and(parse_name)
        .and(parse_body)
        .map(|(((docs_opt, (_, name)), body)| {
            let docs = if let Some(AstNode::DocComment(s)) = docs_opt {
                Some(s)
            } else {
                None
            };
            AstNode::ConceptDef {
                name,
                methods: body,
                docs,
            }
        }))
}
/// Parses impl: optional docs, <gens> impl concept for ty { methods }.
fn parse_impl<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let docs_opt = opt(ws(parse_doc_comment()));
    let parse_generics_opt = opt(ws(parse_generics()));
    let parse_kw = value((), ws(tag("impl")));
    let parse_concept = ws(parse_ident());
    let parse_for_kw = value((), ws(tag("for")));
    let parse_ty = ws(parse_type());
    let parse_body = delimited(ws(tag("{")), many0(ws(parse_method_sig())), ws(tag("}")));
    docs_opt
        .and(parse_generics_opt)
        .and(parse_kw)
        .and(parse_concept)
        .and(parse_for_kw)
        .and(parse_ty)
        .and(parse_body)
        .map(|(docs_opt, (generics_opt, ((((_, concept), _), ty)), body))| {
            let docs = if let Some(AstNode::DocComment(s)) = docs_opt {
                Some(s)
            } else {
                None
            };
            AstNode::ImplBlock {
                generics: generics_opt.unwrap_or(vec![]),
                concept,
                ty,
                body,
                docs,
            }
        })
}
/// Parses enum: optional docs, enum name { variants }.
fn parse_enum<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>> {
    let docs_opt = opt(ws(parse_doc_comment()));
    let parse_kw = value((), ws(tag("enum")));
    let parse_name = ws(parse_ident());
    let parse_variants = delimited(ws(tag("{")), many0(ws(parse_ident())), ws(tag("}")));
    docs_opt
        .and(parse_kw)
        .and(parse_name)
        .and(parse_variants)
        .map(|((docs_opt, ((_, name))), variants)| {
            let docs = if let Some(AstNode::DocComment(s)) = docs_opt {
                Some(s)
            } else {
                None
            };
            AstNode::EnumDef { name, variants, docs }
        })
}
/// Parses struct: optional docs, struct name { fields }.
fn parse_struct<'a>() -> impl Parser<&'a str, Output = AstNode, Error = nom::error::Error<&'a str>>
{
    let docs_opt = opt(ws(parse_doc_comment()));
    let parse_kw = value((), ws(tag("struct")));
    let parse_name = ws(parse_ident());
    let parse_field = ws(parse_ident())
        .and(ws(tag(":")))
        .and(ws(parse_type()))
        .map(|((n, _), t): ((String, &'a str), String)| (n, t));
    let parse_fields = delimited(ws(tag("{")), many0(parse_field), ws(tag("}")));
    docs_opt
        .and(parse_kw)
        .and(parse_name)
        .and(parse_fields)
        .map(|((docs_opt, ((_, name))), fields)| {
            let docs = if let Some(AstNode::DocComment(s)) = docs_opt {
                Some(s)
            } else {
                None
            };
            AstNode::StructDef { name, fields, docs }
        })
}
/// Entry point: Parses multiple top-level items, with recovery.
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    let parser = many0(ws(alt((
        parse_doc_comment(), // Standalone docs
        parse_func(),
        parse_concept(),
        parse_impl(),
        parse_enum(),
        parse_struct(),
    ))));
    match parser(input) {
        Ok(res) => Ok(res),
        Err(e) => {
            // Recovery: try all_consuming on partial
            if let Err(nom::Err::Incomplete(_)) = e {
                Ok((input, vec![]))
            } else {
                Err(e)
            }
        }
    }
}
