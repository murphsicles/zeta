// src/main.rs
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, OptimizationLevel};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};
use inkwell::types::BasicTypeEnum;
use nom::{
    branch::alt,
    bytes::complete::{tag},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list1},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::collections::HashMap;

// Token enum
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Concept, Impl, Fn, Ident(String), Colon, Arrow, LParen, RParen, Comma, Eq, Semi, Lt, Gt,
    Where, For, Mut, BraceOpen, BraceClose, IntLit(i64),
}

fn identifier(input: &str) -> IResult<&str, Token> {
    let ident = recognize(tuple((alt((alpha1, tag("_"))), many0(alt((alphanumeric1, tag("_")))))));
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
    many0(terminated(alt((int_literal, identifier, keyword, symbol)), multispace0))(input)
}

// AST
#[derive(Debug, Clone)]
pub enum AstNode {
    ConceptDef { name: String, params: Vec<String>, methods: Vec<AstNode> },
    Method { name: String, params: Vec<(String, String)>, ret: String },
    ImplBlock { concept: String, ty: String, body: Vec<AstNode> },
    FuncDef { name: String, generics: Vec<String>, params: Vec<(String, String)>, ret: String, body: Vec<AstNode>, where_clause: Option<Vec<(String, String)>> },
    Call { method: String, receiver: String, args: Vec<String> },
    Lit(i64),
    Var(String),
}

fn map_opt(p: impl Fn(&str) -> IResult<&str, Token>) -> impl Fn(&str) -> IResult<&str, Option<String>> {
    move |input| {
        let (i, t) = p(input)?;
        match t {
            Token::Ident(s) => Ok((i, Some(s))),
            _ => Ok((i, None)),
        }
    }
}

fn parse_concept(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((
        tag("concept"), multispace0, map_opt(identifier), multispace0,
        opt(delimited(tag("<"), separated_list1(tag(","), map_opt(identifier)), tag(">"))), multispace0,
        tag("{"), multispace0, many0(parse_method), tag("}"),
    ));
    let (i, (_, _, name, _, params_opt, _, _, _, methods, _)) = parser(input)?;
    Ok((i, AstNode::ConceptDef { name: name.unwrap_or_default(), params: params_opt.unwrap_or_default(), methods }))
}

fn parse_method(input: &str) -> IResult<&str, AstNode> {
    let param_parser = separated_pair(opt(tag("mut")), tag(":"), tuple((map_opt(identifier), multispace0, tag(":"), multispace0, map_opt(identifier))));
    let parser = tuple((
        tag("fn"), multispace0, map_opt(identifier), multispace0,
        delimited(tag("("), separated_list1(tag(","), param_parser), tag(")")), multispace0,
        tag("->"), multispace0, map_opt(identifier), multispace0, tag(";"),
    ));
    let (i, (_, _, name, _, params, _, _, _, ret, _, _)) = parser(input)?;
    let method_params: Vec<(String, String)> = params.into_iter().map(|(mut_opt, (pn, _, _, _, ty))| (pn.unwrap_or_default(), ty.unwrap_or_default())).collect();
    Ok((i, AstNode::Method { name: name.unwrap_or_default(), params: method_params, ret: ret.unwrap_or_default() }))
}

fn parse_impl(input: &str) -> IResult<&str, AstNode> {
    let ty_parser = tuple((map_opt(identifier), multispace0, opt(delimited(tag("<"), separated_list1(tag(","), map_opt(identifier)), tag(">")))));
    let parser = tuple((
        tag("impl"), multispace0, map_opt(identifier), multispace0, ty_parser.clone(), multispace0,
        tag("for"), multispace0, ty_parser, multispace0, tag("{"), multispace0, many0(parse_method), tag("}"),
    ));
    let (i, (_, _, concept, _, _, _, _, _, for_ty, _, _, _, methods, _)) = parser(input)?;
    let full_ty = if let Some(params) = for_ty.2 { format!("{}<{}>", for_ty.0.unwrap_or_default(), params.join(",")) } else { for_ty.0.unwrap_or_default() };
    Ok((i, AstNode::ImplBlock { concept: concept.unwrap_or_default(), ty: full_ty, body: methods }))
}

fn parse_where_clause(input: &str) -> IResult<&str, Vec<(String, String)>> {
    preceded(tag("where"), separated_list1(tag(","), separated_pair(map_opt(identifier), tag(":"), map_opt(identifier))))(input)
}

fn parse_expr(input: &str) -> IResult<&str, AstNode> {
    alt((
        map(int_literal, |t| if let Token::IntLit(n) = t { AstNode::Lit(n) } else { unreachable!() }),
        map(map_opt(identifier), |s| AstNode::Var(s.unwrap_or_default())),
        parse_call,
    ))(input)
}

fn parse_call(input: &str) -> IResult<&str, AstNode> {
    let parser = tuple((map_opt(identifier), multispace0, map_opt(identifier), multispace0, delimited(tag("("), separated_list1(tag(","), map_opt(identifier)), tag(")")), multispace0, tag(";")));
    let (i, (receiver, _, method, _, args_opt, _, _)) = parser(input)?;
    Ok((i, AstNode::Call { method: method.unwrap_or_default(), receiver: receiver.unwrap_or_default(), args: args_opt.unwrap_or_default() }))
}

fn parse_func(input: &str) -> IResult<&str, AstNode> {
    let generics_parser = opt(delimited(tag("<"), separated_list1(tag(","), map_opt(identifier)), tag(">")));
    let param_parser = separated_pair(opt(tag("mut")), tag(":"), tuple((map_opt(identifier), multispace0, tag(":"), multispace0, map_opt(identifier))));
    let parser = tuple((
        tag("fn"), multispace0, map_opt(identifier), multispace0, generics_parser, multispace0,
        delimited(tag("("), separated_list1(tag(","), param_parser), tag(")")), multispace0,
        tag("->"), multispace0, map_opt(identifier), multispace0, opt(parse_where_clause), multispace0,
        tag("{"), multispace0, many0(parse_expr), tag("}"),
    ));
    let (i, (_, _, name, _, generics_opt, _, params, _, _, _, ret, _, where_opt, _, _, _, body, _)) = parser(input)?;
    let generics = generics_opt.unwrap_or_default();
    let func_params: Vec<(String, String)> = params.into_iter().map(|(mut_opt, (pn, _, _, _, ty))| (pn.unwrap_or_default(), ty.unwrap_or_default())).collect();
    Ok((i, AstNode::FuncDef {
        name: name.unwrap_or_default(), generics, params: func_params, ret: ret.unwrap_or_default(),
        body, where_clause: where_opt,
    }))
}

pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    many0(alt((parse_func, parse_impl, parse_concept)))(input)
}

// Resolver
#[derive(Debug, Clone)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
}

impl Resolver {
    pub fn new() -> Self { Self { concepts: HashMap::new(), impls: HashMap::new(), funcs: HashMap::new() } }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { name, .. } => { self.concepts.insert(name, ast); }
            AstNode::ImplBlock { concept, ty, .. } => { self.impls.insert((concept, ty), ast); }
            AstNode::FuncDef { name, .. } => { self.funcs.insert(name, ast); }
            _ => {}
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<&AstNode> {
        self.impls.get(&(concept.to_string(), ty.to_string()))
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        for ast in asts {
            if let AstNode::FuncDef { where_clause, body, .. } = ast {
                if let Some(bounds) = where_clause {
                    for (ty, concept) in bounds { if self.resolve_impl(&concept, &ty).is_none() { return false; } }
                }
                for node in body {
                    if let AstNode::Call { method, .. } = node { if !self.has_method("Addable", "i32", method) { return false; } }
                }
            }
        }
        true
    }

    fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        self.resolve_impl(concept, ty).map_or(false, |impl_ast| {
            if let AstNode::ImplBlock { body, .. } = impl_ast { body.iter().any(|m| if let AstNode::Method { name, .. } = m { name == method } else { false }) } else { false }
        })
    }
}

// LLVM Codegen
pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: inkwell::types::IntType<'ctx>,
    add_fn: Option<FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();
        Self { context, module, builder, execution_engine: None, i32_type, add_fn: None }
    }

    pub fn gen_add_impl(&mut self) {
        let fn_type = self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
        let fn_val = self.module.add_function("add_i32", fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        let x = fn_val.get_nth_param(0).unwrap().into_int_value();
        let y = fn_val.get_nth_param(1).unwrap().into_int_value();
        let ret = self.builder.build_int_add(x, y, "sum");
        self.builder.build_return(Some(&ret.into()));
        self.add_fn = Some(fn_val);
    }

    pub fn gen_func(&mut self, func: &AstNode, param_map: &mut HashMap<String, IntValue<'ctx>>) -> Option<FunctionValue<'ctx>> {
        if let AstNode::FuncDef { name, params, ret, body, .. } = func {
            if *ret != "i32" { return None; }
            let param_types: Vec<BasicTypeEnum<'ctx>> = params.iter().map(|(_, t)| if *t == "i32" { self.i32_type.into() } else { panic!("Unsupported type") }).collect();
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);
            let mut args = Vec::new();
            for (i, param) in params.iter().enumerate() {
                let arg = fn_val.get_nth_param(i as u32).unwrap().into_int_value();
                args.push(arg);
                param_map.insert(param.0.clone(), arg);
            }
            let mut last_val = self.i32_type.const_int(0, false);
            for node in body.iter() {
                if let Some(val) = self.gen_stmt(node, param_map) {
                    last_val = val;
                }
            }
            self.builder.build_return(Some(&last_val.into()));
            Some(fn_val)
        } else { None }
    }

    fn gen_stmt(&mut self, node: &AstNode, param_map: &HashMap<String, IntValue<'ctx>>) -> Option<IntValue<'ctx>> {
        match node {
            AstNode::Call { method, receiver, args } if *method == "add" => {
                if let (Some(recv_val), Some(arg_name)) = (param_map.get(receiver), args.first()) {
                    if let Some(arg_val) = param_map.get(arg_name) {
                        if let Some(add) = &self.add_fn {
                            let call_site = self.builder.build_call(add, &[(*recv_val).into(), (*arg_val).into()], "add_call");
                            if let Some(bv) = call_site.try_as_basic_value().left() {
                                return bv.into_int_value().ok();
                            }
                        }
                    }
                }
                None
            }
            AstNode::Lit(n) => Some(self.i32_type.const_int(*n as u64, false)),
            AstNode::Var(v) => param_map.get(v).cloned(),
            _ => None,
        }
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        self.execution_engine = Some(ee.clone());
        Ok(ee)
    }

    pub fn get_fn<F>(&self, name: &str) -> Option<JitFunction<F>> where F: 'static + Copy + inkwell::supports::ToFromPrimitive {
        self.execution_engine.as_ref().and_then(|ee| ee.get_function(name).ok())
    }
}

// Main
pub fn compile_and_run_zeta(input: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let (_, asts) = parse_zeta(input)?;
    let mut resolver = Resolver::new();
    for ast in &asts { resolver.register(ast.clone()); }
    if !resolver.typecheck(&asts) { return Err("Typecheck failed".into()); }

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta");
    codegen.gen_add_impl();
    let mut param_map = HashMap::new();
    for ast in asts.iter().filter(|a| matches!(a, AstNode::FuncDef { .. })) {
        codegen.gen_func(ast, &mut param_map);
    }
    let ee = codegen.finalize_and_jit()?;

    unsafe {
        type UseAddFn = unsafe extern "C" fn(i32, i32) -> i32;
        if let Some(use_add) = codegen.get_fn::<UseAddFn>("use_add") {
            Ok(use_add.call(5, 3) as i32)
        } else {
            Err("use_add not found".into())
        }
    }
}

fn main() {
    let code = r#"
concept Addable<Rhs=Self> {
    fn add(self: Self, rhs: Rhs) -> Self;
}

impl Addable for i32 {
    fn add(self: i32, rhs: i32) -> i32;
}

fn use_add<T>(a: T, b: T) -> i32 where T: Addable {
    a.add(b);
}
"#;
    match compile_and_run_zeta(code) {
        Ok(res) => println!("Result: {}", res),
        Err(e) => eprintln!("Error: {}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = r#"concept Addable { fn add(self: Self, rhs: Self) -> Self; }"#;
        let result = parse_zeta(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_typecheck() {
        let code = r#"
concept Addable { fn add(self: Self, rhs: Self) -> Self; }
impl Addable for i32 { fn add(self: i32, rhs: i32) -> i32; }
fn use_add<T>(a: T, b: T) -> i32 where T: Addable { a.add(b); }
"#;
        let (_, asts) = parse_zeta(code).unwrap();
        let mut resolver = Resolver::new();
        for ast in &asts { resolver.register(ast.clone()); }
        assert!(resolver.typecheck(&asts));
    }
}
