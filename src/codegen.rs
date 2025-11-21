// src/codegen.rs
use crate::ast::AstNode;
use crate::resolver::Resolver;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, IntValue, VectorValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: inkwell::types::IntType<'ctx>,
    i32x4_type: inkwell::types::VectorType<'ctx>,
    locals: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();
        let i32x4_type = i32_type.vec_type(4);

        let mut codegen = Self {
            context,
            module,
            builder,
            execution_engine: None,
            i32_type,
            i32x4_type,
            locals: HashMap::new(),
        };

        codegen.create_intrinsics();
        codegen
    }

    fn create_intrinsics(&mut self) {
        macro_rules! scalar_intrin {
            ($name:ident, $op:ident) => {
                let ty = self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
                let f = self.module.add_function(stringify!($name), ty, None);
                let entry = self.context.append_basic_block(f, "entry");
                self.builder.position_at_end(entry);
                let x = f.get_nth_param(0).unwrap().into_int_value();
                let y = f.get_nth_param(1).unwrap().into_int_value();
                let res = self.builder.build_int_$op(x, y, stringify!($op)).unwrap();
                self.builder.build_return(Some(&res)).unwrap();
            };
        }

        macro_rules! simd_intrin {
            ($name:ident, $op:ident) => {
                let ty = self.i32x4_type.fn_type(&[self.i32x4_type.into(), self.i32x4_type.into()], false);
                let f = self.module.add_function(stringify!($name), ty, None);
                let entry = self.context.append_basic_block(f, "entry");
                self.builder.position_at_end(entry);
                let x = f.get_nth_param(0).unwrap().into_vector_value();
                let y = f.get_nth_param(1).unwrap().into_vector_value();
                let res = self.builder.build_int_$op(x, y, stringify!($op)).unwrap();
                self.builder.build_return(Some(&res)).unwrap();
            };
        }

        scalar_intrin!(add_i32, add);
        simd_intrin!(add_i32x4, add);
        scalar_intrin!(mul_i32, mul);
        simd_intrin!(mul_i32x4, mul);
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
        if let AstNode::Call { receiver, method, .. } = node {
            let recv_val = self.load_var(receiver);
            let arg_val = recv_val; // placeholder

            let call = match method.as_str() {
                "add" => self.builder.build_call(
                    self.module.get_function("add_i32").unwrap(),
                    &[recv_val.into(), arg_val.into()],
                    "add",
                ),
                _ => return,
            };

            if let Some(bv) = call.try_as_basic_value().left() {
                if let Some(ptr) = self.locals.get(receiver) {
                    self.builder.build_store(*ptr, bv).unwrap();
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
        F: inkwell::execution_engine::UnsafeFunctionPointer,
    {
        unsafe { self.execution_engine.as_ref()?.get_function(name).ok() }
    }
}
