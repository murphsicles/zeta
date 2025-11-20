// src/codegen.rs
use crate::ast::AstNode;
use crate::mir::{Mir, MirStmt, SemiringOp};
use crate::resolver::Resolver;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};
use inkwell::types::{IntType, VectorType};
use inkwell::OptimizationLevel;
use inkwell::passes::PassManager;
use std::collections::HashMap;
use std::error::Error;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    fpm: PassManager<FunctionValue<'ctx>>,
    mpm: PassManager<Module<'ctx>>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: IntType<'ctx>,
    i32x4_type: VectorType<'ctx>,
    locals: HashMap<u32, inkwell::values::PointerValue<'ctx>>,
    enable_simd: bool,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();
        let i32x4_type = i32_type.vec_type(4);

        let fpm = PassManager::create(&module);
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_dead_store_elimination_pass();
        fpm.add_memcpy_opt_pass();
        fpm.add_correlated_value_propagation_pass();
        fpm.add_aggressive_dce_pass();
        fpm.add_loop_vectorize_pass();
        fpm.add_slp_vectorize_pass();

        let mpm = PassManager::create(());
        mpm.add_global_optimizer_pass();
        mpm.add_global_dce_pass();

        Self {
            context,
            module,
            builder,
            fpm,
            mpm,
            execution_engine: None,
            i32_type,
            i32x4_type,
            locals: HashMap::new(),
            enable_simd: true,
        }
    }

    pub fn gen_mir(&mut self, mir: &Mir) {
        for stmt in &mir.stmts {
            self.gen_stmt(stmt);
        }
    }

    fn gen_stmt(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let value = match rhs {
                    crate::mir::MirExpr::Var(v) => self.load_local(*v),
                    crate::mir::MirExpr::Lit(n) => self.i32_type.const_int(*n as u64, false).into(),
                    _ => self.i32_type.const_int(0, false).into(),
                };
                let ptr = self.locals.entry(*lhs).or_insert_with(|| {
                    self.builder
                        .build_alloca(self.i32_type, &format!("local_{}", lhs))
                        .unwrap()
                });
                self.builder.build_store(*ptr, value).unwrap();
            }
            MirStmt::SemiringFold { op, values, result } => {
                let count = values.len();
                let mut acc = self.load_local(values[0]);

                if count >= 4 && self.enable_simd {
                    for chunk in values[1..].chunks(4) {
                        let mut elems = [acc.into_int_value(); 4];
                        for (i, &v) in chunk.iter().enumerate() {
                            elems[i] = self.load_local(v).into_int_value();
                        }
                        let vec = self
                            .builder
                            .elf
                            .build_vector_from_elements(elems.to_vec())
                            .unwrap();
                        acc = match op {
                            SemiringOp::Add => self
                                .builder
                                .build_int_add(acc.into_int_value(), vec.into_vector_value(), "fold_add_simd")
                                .unwrap()
                                .into(),
                            SemiringOp::Mul => self
                                .builder
                                .build_int_mul(acc.into_int_value(), vec.into_vector_value(), "fold_mul_simd")
                                .unwrap()
                                .into(),
                        };
                    }
                } else {
                    for &v in &values[1..] {
                        let rhs = self.load_local(v);
                        acc = match op {
                            SemiringOp::Add => self
                                .builder
                                .build_int_add(acc.into_int_value(), rhs.into_int_value(), "fold_add")
                                .unwrap()
                                .into(),
                            SemiringOp::Mul => self
                                .builder
                                .build_int_mul(acc.into_int_value(), rhs.into_int_value(), "fold_mul")
                                .unwrap()
                                .into(),
                        };
                    }
                }

                let ptr = self.locals.entry(*result).or_insert_with(|| {
                    self.builder
                        .build_alloca(self.i32_type, &format!("result_{}", result))
                        .unwrap()
                });
                self.builder.build_store(*ptr, acc).unwrap();
            }
            MirStmt::Return { val } => {
                let v = self.load_local(*val);
                self.builder.build_return(Some(&v)).unwrap();
            }
            _ => {}
        }
    }

    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = self.locals.get(&id).copied().unwrap_or_else(|| {
            self.builder
                .build_alloca(self.i32_type, &format!("tmp_{}", id))
                .unwrap()
        });
        self.builder
            .build_load(self.i32_type, ptr, &format!("load_{}", id))
            .unwrap()
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        self.mpm.run_on(&self.module);
        let ee = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)?;
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
