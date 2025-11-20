use crate::ast::AstNode;
use crate::resolver::Resolver;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: inkwell::types::IntType<'ctx>,
    add_i32_fn: Option<FunctionValue<'ctx>>,
    locals: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();

        Self {
            context,
            module,
            builder,
            execution_engine: None,
            i32_type,
            add_i32_fn: None,
            locals: HashMap::new(),
        }
    }

    pub fn gen_intrinsics(&mut self) {
        let fn_type = self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
        let add_fn = self.module.add_function("add_i32", fn_type, None);
        let entry = self.context.append_basic_block(add_fn, "entry");
        self.builder.position_at_end(entry);
        let x = add_fn.get_nth_param(0).unwrap().into_int_value();
        let y = add_fn.get_nth_param(1).unwrap().into_int_value();
        let sum = self.builder.build_int_add(x, y, "sum");
        self.builder.build_return(Some(&sum.into()));
        self.add_i32_fn = Some(add_fn);
    }

    fn gen_stmt(&mut self, node: &AstNode, locals: &mut HashMap<String, PointerValue<'ctx>>, resolver: &Resolver) {
        match node {
            AstNode::Let { name, rhs, .. } => {
                let val = self.gen_expr(rhs, locals, resolver).unwrap_or(self.i32_type.const_zero().into());
                let alloca = self.builder.build_alloca(self.i32_type, name).unwrap();
                self.builder.build_store(alloca, val).unwrap();
                locals.insert(name.clone(), alloca);
            }
            AstNode::ExprStmt(expr) => {
                let _ = self.gen_expr(expr, locals, resolver);
            }
            _ => {}
        }
    }

    fn gen_expr(&mut self, node: &AstNode, locals: &HashMap<String, PointerValue<'ctx>>, resolver: &Resolver) -> Option<BasicValueEnum<'ctx>> {
        match node {
            AstNode::Lit(n) => Some(self.i32_type.const_int(*n as u64, false).into()),
            AstNode::Var(name) => locals.get(name).map(|&ptr| self.builder.build_load(self.i32_type, ptr, name).unwrap()),
            AstNode::Call { receiver, method, args } => {
                let recv_val = self.gen_expr(receiver, locals, resolver)?;
                let arg_vals: Vec<_> = args.iter()
                    .map(|a| self.gen_expr(a, locals, resolver).unwrap())
                    .collect();
                if method == "add" && resolver.has_method("Addable", &format!("{:?}", receiver), "add") {
                    if let Some(add_fn) = self.add_i32_fn {
                        let call = self.builder.build_call(add_fn, &[recv_val, arg_vals[0]], "").unwrap();
                        call.try_as_basic_value().left()
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn gen_func(&mut self, ast: &AstNode, resolver: &Resolver) -> Option<FunctionValue<'ctx>> {
        if let AstNode::FuncDef { name, params, body, ret_expr, .. } = ast {
            let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = params.iter().map(|_| self.i32_type.into()).collect();
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);

            let mut locals = HashMap::new();
            for (i, (pname, _)) in params.iter().enumerate() {
                let param = fn_val.get_nth_param(i as u32)?.into_int_value();
                let alloca = self.builder.build_alloca(self.i32_type, pname).unwrap();
                self.builder.build_store(alloca, param).unwrap();
                locals.insert(pname.clone(), alloca);
            }

            for stmt in body {
                self.gen_stmt(stmt, &mut locals, resolver);
            }

            let ret_val = ret_expr
                .as_ref()
                .and_then(|e| self.gen_expr(e, &locals, resolver))
                .unwrap_or(self.i32_type.const_zero().into());
            self.builder.build_return(Some(&ret_val));

            Some(fn_val)
        } else {
            None
        }
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        self.execution_engine = Some(ee.clone());
        Ok(ee)
    }

    pub fn get_fn<F>(&self, name: &str) -> Option<JitFunction<F>>
    where
        F: inkwell::execution_engine::UnsafeFunctionPointer,
    {
        self.execution_engine.as_ref()?.get_function(name).ok()
    }
}
