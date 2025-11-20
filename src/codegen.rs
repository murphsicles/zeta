// src/codegen.rs
use crate::ast::AstNode;
use crate::resolver::Resolver;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
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
    add_i32_fn: Option<FunctionValue<'ctx>>,
    add_i32x4_fn: Option<FunctionValue<'ctx>>,
    sub_i32_fn: Option<FunctionValue<'ctx>>,
    sub_i32x4_fn: Option<FunctionValue<'ctx>>,
    mul_i32_fn: Option<FunctionValue<'ctx>>,
    mul_i32x4_fn: Option<FunctionValue<'ctx>>,
    div_i32_fn: Option<FunctionValue<'ctx>>,
    div_i32x4_fn: Option<FunctionValue<'ctx>>,
    rem_i32_fn: Option<FunctionValue<'ctx>>,
    rem_i32x4_fn: Option<FunctionValue<'ctx>>,
    shl_i32_fn: Option<FunctionValue<'ctx>>,
    shl_i32x4_fn: Option<FunctionValue<'ctx>>,
    shr_i32_fn: Option<FunctionValue<'ctx>>,
    shr_i32x4_fn: Option<FunctionValue<'ctx>>,
    and_i32_fn: Option<FunctionValue<'ctx>>,
    and_i32x4_fn: Option<FunctionValue<'ctx>>,
    or_i32_fn: Option<FunctionValue<'ctx>>,
    or_i32x4_fn: Option<FunctionValue<'ctx>>,
    xor_i32_fn: Option<FunctionValue<'ctx>>,
    xor_i32x4_fn: Option<FunctionValue<'ctx>>,
    locals: HashMap<String, PointerValue<'ctx>>,
}

macro_rules! scalar_intrin {
    ($name:ident, $op:ident) => {
        let ty = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);
        let f = module.add_function(stringify!($name), ty, None);
        let entry = context.append_basic_block(f, "entry");
        builder.position_at_end(entry);
        let x = f.get_nth_param(0).unwrap().into_int_value();
        let y = f.get_nth_param(1).unwrap().into_int_value();
        let res = builder.build_int_$op(x, y, stringify!($op)).unwrap();
        builder.build_return(Some(&res)).unwrap();
        $name: Some(f)
    };
}

macro_rules! simd_intrin {
    ($name:ident, $op:ident) => {
        let ty = i32x4_type.fn_type(&[i32x4_type.into(), i32x4_type.into()], false);
        let f = module.add_function(stringify!($name), ty, None);
        let entry = context.append_basic_block(f, "entry");
        builder.position_at_end(entry);
        let x = f.get_nth_param(0).unwrap().into_vector_value();
        let y = f.get_nth_param(1).unwrap().into_vector_value();
        let res = builder.build_int_$op(x, y, stringify!($op)).unwrap();
        builder.build_return(Some(&res)).unwrap();
        $name: Some(f)
    };
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();
        let i32x4_type = i32_type.vec_type(4);

        scalar_intrin!(add_i32_fn, add);
        simd_intrin!(add_i32x4_fn, add);

        scalar_intrin!(sub_i32_fn, sub);
        simd_intrin!(sub_i32x4_fn, sub);

        scalar_intrin!(mul_i32_fn, mul);
        simd_intrin!(mul_i32x4_fn, mul);

        scalar_intrin!(div_i32_fn, signed_div);
        simd_intrin!(div_i32x4_fn, signed_div);

        scalar_intrin!(rem_i32_fn, signed_rem);
        simd_intrin!(rem_i32x4_fn, signed_rem);

        scalar_intrin!(shl_i32_fn, shl);
        simd_intrin!(shl_i32x4_fn, shl);

        scalar_intrin!(shr_i32_fn, ashr);
        simd_intrin!(shr_i32x4_fn, ashr);

        scalar_intrin!(and_i32_fn, and);
        simd_intrin!(and_i32x4_fn, and);

        scalar_intrin!(or_i32_fn, or);
        simd_intrin!(or_i32x4_fn, or);

        scalar_intrin!(xor_i32_fn, xor);
        simd_intrin!(xor_i32x4_fn, xor);

        Self {
            context,
            module,
            builder,
            execution_engine: None,
            i32_type,
            i32x4_type,
            add_i32_fn,
            add_i32x4_fn,
            sub_i32_fn,
            sub_i32x4_fn,
            mul_i32_fn,
            mul_i32x4_fn,
            div_i32_fn,
            div_i32x4_fn,
            rem_i32_fn,
            rem_i32x4_fn,
            shl_i32_fn,
            shl_i32x4_fn,
            shr_i32_fn,
            shr_i32x4_fn,
            and_i32_fn,
            and_i32x4_fn,
            or_i32_fn,
            or_i32x4_fn,
            xor_i32_fn,
            xor_i32x4_fn,
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
            let is_simd = receiver.contains("vec") || (!args.is_empty() && args[0].contains("vec"));

            let (scalar_fn, simd_fn) = match method.as_str() {
                "add" => (self.add_i32_fn, self.add_i32x4_fn),
                "sub" => (self.sub_i32_fn, self.sub_i32x4_fn),
                "mul" => (self.mul_i32_fn, self.mul_i32x4_fn),
                "div" => (self.div_i32_fn, self.div_i32x4_fn),
                "rem" => (self.rem_i32_fn, self.rem_i32x4_fn),
                "shl" => (self.shl_i32_fn, self.shl_i32x4_fn),
                "shr" => (self.shr_i32_fn, self.shr_i32x4_fn),
                "and" => (self.and_i32_fn, self.and_i32x4_fn),
                "or"  => (self.or_i32_fn,  self.or_i32x4_fn),
                "xor" => (self.xor_i32_fn, self.xor_i32x4_fn),
                _ => return,
            };

            let op_fn = if is_simd { simd_fn } else { scalar_fn };
            if let Some(op_fn) = op_fn {
                let recv_val = self.load_var(receiver);
                let arg_val = if args.is_empty() {
                    recv_val
                } else {
                    self.load_var(&args[0])
                };
                let call = self.builder.build_call(op_fn, &[recv_val.into(), arg_val.into()], method).unwrap();

                if let Some(bv) = call.try_as_basic_value().left() {
                    if let Some(ptr) = self.locals.get(receiver) {
                        self.builder.build_store(*ptr, bv).unwrap();
                    }
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
