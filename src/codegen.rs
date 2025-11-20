// src/codegen.rs
use crate::ast::AstNode;
use crate::resolver::Resolver;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::types::IntType;
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: IntType<'ctx>,
    add_i32_fn: Option<FunctionValue<'ctx>>,
    locals: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();

        let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);
        let add_i32_fn = module.add_function("add_i32", fn_type, None);
        let entry = context.append_basic_block(add_i32_fn, "entry");
        builder.position_at_end(entry);
        let x = add_i32_fn.get_nth_param(0).unwrap().into_int_value();
        let y = add_i32_fn.get_nth_param(1).unwrap().into_int_value();
        let sum = builder.build_int_add(x, y, "sum").unwrap();
        builder.build_return(Some(&sum)).unwrap();

        Self {
            context,
            module,
            builder,
            execution_engine: None,
            i32_type,
            add_i32_fn: Some(add_i32_fn),
            locals: HashMap::new(),
        }
    }

    pub fn gen_func(&mut self, ast: &AstNode, _resolver: &Resolver) {
        if let AstNode::FuncDef { name, params, body, .. } = ast {
            let param_types = vec![self.i32_type.into(); params.len()];
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);

            for (i, (pname, _)) in params.iter().enumerate() {
                let param = fn_val.get_nth_param(i as u32).unwrap();
                let alloca = self.builder.build_alloca(self.i32_type, pname).unwrap();
                self.builder.build_store(alloca, param).unwrap();
                self.locals.insert(pname.clone(), alloca);
            }

            for stmt in body {
                self.gen_stmt(stmt);
            }

            let zero = self.i32_type.const_int(0, false);
            self.builder.build_return(Some(&zero)).unwrap();
        }
    }

    fn gen_stmt(&mut self, node: &AstNode) {
        if let AstNode::Call { receiver, method, args } = node {
            if method == "add" {
                if let Some(add_fn) = self.add_i32_fn {
                    let recv_val = self.load_var(receiver);
                    let arg_val = if args.is_empty() {
                        recv_val
                    } else {
                        self.load_var(&args[0])
                    };
                    let call = self.builder
                        .build_call(add_fn, &[recv_val.into(), arg_val.into()], "addtmp")
                        .unwrap();

                    if let Some(bv) = call.try_as_basic_value().left() {
                        if let Some(int_val) = bv.into_int_value() {
                            if let Some(ptr) = self.locals.get(receiver) {
                                self.builder.build_store(*ptr, int_val).unwrap();
                            }
                        }
                    }
                }
            }
        }
    }

    fn load_var(&self, name: &str) -> IntValue<'ctx> {
        let ptr = self.locals.get(name).copied().unwrap_or_else(|| {
            self.builder.build_alloca(self.i32_type, name).unwrap()
        });
        self.builder.build_load(self.i32_type, ptr, name).unwrap().into_int_value()
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        self.execution_engine = Some(ee.clone());
        Ok(ee)
    }

    pub fn get_fn<F>(&self, name: &str) -> Option<JitFunction<F>>
    where
        F: UnsafeFunctionPointer,
    {
        unsafe { self.execution_engine.as_ref()?.get_function(name).ok() }
    }
}

pub fn compile_and_run_zeta(input: &str) -> Result<i32, Box<dyn Error>> {
    let (_, asts) = crate::parser::parse_zeta(input).map_err(|e| format!("{:?}", e))?;
    let resolver = Resolver::new();

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta");

    for ast in asts.iter().filter(|a| matches!(a, AstNode::FuncDef { .. })) {
        codegen.gen_func(ast, &resolver);
    }

    let ee = codegen.finalize_and_jit()?;

    type MainFn = unsafe extern "C" fn() -> i32;
    unsafe {
        match ee.get_function::<MainFn>("main") {
            Ok(f) => Ok(f.call()),
            Err(_) => Ok(0),
        }
    }
}
