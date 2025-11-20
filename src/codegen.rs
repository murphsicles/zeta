use crate::ast::AstNode;
use crate::resolver::Resolver;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
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
        let f = self.module.add_function("add_i32", fn_type, None);
        let bb = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(bb);
        let x = f.get_nth_param(0).unwrap().into_int_value();
        let y = f.get_nth_param(1).unwrap().into_int_value();
        let sum = self.builder.build_int_add(x, y, "sum");
        self.builder.build_return(Some(&sum));
        self.add_i32_fn = Some(f);
    }

    fn gen_expr(&mut self, node: &AstNode, locals: &HashMap<String, PointerValue<'ctx>>) -> Option<BasicValueEnum<'ctx>> {
        match node {
            AstNode::Lit(v) => Some(self.i32_type.const_int(*v as u64, false).into()),
            AstNode::Var(name) => locals.get(name).map(|p| self.builder.build_load(self.i32_type, *p, name).unwrap()),
            AstNode::Call { receiver, method, args } => {
                let recv = self.gen_expr(receiver, locals)?;
                let arg_vals: Vec<_> = args.iter().map(|a| self.gen_expr(a, locals).unwrap()).collect();
                if method == "add" {
                    if let Some(add) = self.add_i32_fn {
                        let meta: Vec<BasicMetadataValueEnum<'ctx>> = vec![recv.into(), arg_vals[0].into()];
                        let call = self.builder.build_call(add, &meta, "").unwrap();
                        call.try_as_basic_value().and_then(|v| v.left())
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

    fn gen_stmt(&mut self, node: &AstNode, locals: &mut HashMap<String, PointerValue<'ctx>>) {
        if let AstNode::Let { name, rhs, .. } = node {
            let val = self.gen_expr(rhs, locals).unwrap_or(self.i32_type.const_zero().into());
            let ptr = self.builder.build_alloca(self.i32_type, name).unwrap();
            self.builder.build_store(ptr, val);
            locals.insert(name.clone(), ptr);
        }
    }

    pub fn gen_func(&mut self, ast: &AstNode) {
        if let AstNode::FuncDef { name, params, body, ret_expr, .. } = ast {
            let param_types: Vec<_> = params.iter().map(|_| self.i32_type.into()).collect();
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let f = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(f, "entry");
            self.builder.position_at_end(entry);

            let mut locals = HashMap::new();
            for (i, (pname, _)) in params.iter().enumerate() {
                let param = f.get_nth_param(i as u32).unwrap().into_int_value();
                let ptr = self.builder.build_alloca(self.i32_type, pname).unwrap();
                self.builder.build_store(ptr, param);
                locals.insert(pname.clone(), ptr);
            }

            for stmt in body {
                self.gen_stmt(stmt, &mut locals);
            }

            let ret = ret_expr.as_ref()
                .and_then(|e| self.gen_expr(e, &locals))
                .unwrap_or(self.i32_type.const_zero().into());
            self.builder.build_return(Some(&ret));
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
        self.execution_engine.as_ref().and_then(|ee| unsafe { ee.get_function(name).ok() })
    }
}
