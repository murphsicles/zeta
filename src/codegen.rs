// src/codegen.rs
use crate::ast::AstNode;
use crate::resolver::Resolver;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{FunctionValue, IntValue, VectorValue, BasicValueEnum};
use inkwell::types::{IntType, VectorType};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: IntType<'ctx>,
    i32x4_type: VectorType<'ctx>,
    locals: HashMap<String, inkwell::values::PointerValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();
        let i32x4_type = i32_type.vec_type(4);

        Self {
            context,
            module,
            builder,
            execution_engine: None,
            i32_type,
            i32x4_type,
            locals: HashMap::new(),
        }
    }

    fn create_intrin_scalar(
        &mut self,
        name: &str,
        op: impl Fn(&Builder<'ctx>, IntValue<'ctx>, IntValue<'ctx>, &str) -> IntValue<'ctx>,
        tmp: &str,
    ) -> FunctionValue<'ctx> {
        let fn_type = self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
        let func = self.module.add_function(name, fn_type, None);
        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        let x = func.get_nth_param(0).unwrap().into_int_value();
        let y = func.get_nth_param(1).unwrap().into_int_value();
        let res = op(&self.builder, x, y, tmp);
        self.builder.build_return(Some(&res)).unwrap();
        func
    }

    fn create_intrin_simd(
        &mut self,
        name: &str,
        op: impl Fn(&Builder<'ctx>, VectorValue<'ctx>, VectorValue<'ctx>, &str) -> VectorValue<'ctx>,
        tmp: &str,
    ) -> FunctionValue<'ctx> {
        let fn_type = self.i32x4_type.fn_type(&[self.i32x4_type.into(), self.i32x4_type.into()], false);
        let func = self.module.add_function(name, fn_type, None);
        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        let x = func.get_nth_param(0).unwrap().into_vector_value();
        let y = func.get_nth_param(1).unwrap().into_vector_value();
        let res = op(&self.builder, x, y, tmp);
        self.builder.build_return(Some(&res)).unwrap();
        func
    }

    pub fn build_intrinsics(&mut self) {
        self.create_intrin_scalar("intrin_add_i32", Builder::build_int_add, "addtmp");
        self.create_intrin_simd("intrin_add_i32x4", Builder::build_int_add, "addtmp");

        self.create_intrin_scalar("intrin_sub_i32", Builder::build_int_sub, "subtmp");
        self.create_intrin_simd("intrin_sub_i32x4", Builder::build_int_sub, "subtmp");

        self.create_intrin_scalar("intrin_mul_i32", Builder::build_int_mul, "multmp");
        self.create_intrin_simd("intrin_mul_i32x4", Builder::build_int_mul, "multmp");

        self.create_intrin_scalar("intrin_sdiv_i32", Builder::build_int_signed_div, "divtmp");
        self.create_intrin_simd("intrin_sdiv_i32x4", Builder::build_int_signed_div, "divtmp");

        self.create_intrin_scalar("intrin_srem_i32", Builder::build_int_signed_rem, "remtmp");
        self.create_intrin_simd("intrin_srem_i32x4", Builder::build_int_signed_rem, "remtmp");

        self.create_intrin_scalar("intrin_shl_i32", Builder::build_left_shift, "shltmp");
        self.create_intrin_simd("intrin_shl_i32x4", Builder::build_left_shift, "shltmp");

        self.create_intrin_scalar("intrin_ashr_i32", |b, x, y, n| b.build_right_shift(x, y, true, n).unwrap(), "ashrtmp");
        self.create_intrin_simd("intrin_ashr_i32x4", |b, x, y, n| b.build_right_shift(x, y, true, n).unwrap(), "ashrtmp");

        self.create_intrin_scalar("intrin_and_i32", Builder::build_and, "andtmp");
        self.create_intrin_simd("intrin_and_i32x4", Builder::build_and, "andtmp");

        self.create_intrin_scalar("intrin_or_i32", Builder::build_or, "ortmp");
        self.create_intrin_simd("intrin_or_i32x4", Builder::build_or, "ortmp");

        self.create_intrin_scalar("intrin_xor_i32", Builder::build_xor, "xortmp");
        self.create_intrin_simd("intrin_xor_i32x4", Builder::build_xor, "xortmp");
    }

    pub fn gen_func(&mut self, ast: &AstNode, _resolver: &Resolver) {
        if let AstNode::FuncDef { name, params, body, .. } = ast {
            let param_types: Vec<_> = params.iter().map(|_| self.i32_type.into()).collect();
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
            let is_simd = receiver.contains("vec") || (!args.is_empty() && args[0].contains("vec"));
            let intrin_name = match method.as_str() {
                "add" => if is_simd { "intrin_add_i32x4" } else { "intrin_add_i32" },
                "sub" => if is_simd { "intrin_sub_i32x4" } else { "intrin_sub_i32" },
                "mul" => if is_simd { "intrin_mul_i32x4" } else { "intrin_mul_i32" },
                "div" => if is_simd { "intrin_sdiv_i32x4" } else { "intrin_sdiv_i32" },
                "rem" => if is_simd { "intrin_srem_i32x4" } else { "intrin_srem_i32" },
                "shl" => if is_simd { "intrin_shl_i32x4" } else { "intrin_shl_i32" },
                "shr" => if is_simd { "intrin_ashr_i32x4" } else { "intrin_ashr_i32" },
                "and" => if is_simd { "intrin_and_i32x4" } else { "intrin_and_i32" },
                "or"  => if is_simd { "intrin_or_i32x4" } else { "intrin_or_i32" },
                "xor" => if is_simd { "intrin_xor_i32x4" } else { "intrin_xor_i32" },
                _ => return,
            };

            let op_fn = self.module.get_function(intrin_name).unwrap();
            let recv_val = self.load_var(receiver);
            let arg_val = if args.is_empty() {
                recv_val
            } else {
                self.load_var(&args[0])
            };

            let call = self.builder
                .build_call(op_fn, &[recv_val.into(), arg_val.into()], "calltmp")
                .unwrap();

            if let Some(result) = call.try_as_basic_value().and_then(BasicValueEnum::into_int_value).ok() {
                if let Some(ptr) = self.locals.get(receiver) {
                    self.builder.build_store(*ptr, result).unwrap();
                }
            }
        }
    }

    fn load_var(&self, name: &str) -> BasicValueEnum<'ctx> {
        let ptr = self.locals.get(name).copied().unwrap_or_else(|| {
            self.builder.build_alloca(self.i32_type, name).unwrap()
        });
        self.builder.build_load(self.i32_type, ptr, name).unwrap()
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
    codegen.build_intrinsics();

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
