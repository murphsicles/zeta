// src/codegen.rs
use crate::mir::{Mir, MirStmt, MirExpr, SemiringOp};
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Module, Linkage};
use inkwell::builder::Builder;
use inkwell::values::{PointerValue, BasicValueEnum};
use inkwell::types::IntType;
use inkwell::OptimizationLevel;
use inkwell::AddressSpace;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}

extern "C" fn host_free(ptr: *mut std::ffi::c_void) {
    if !ptr.is_null() {
        unsafe { libc::free(ptr) }
    }
}

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    i64_type: IntType<'ctx>,
    #[allow(dead_code)]
    ptr_type: inkwell::types::PointerType<'ctx>,
    locals: HashMap<u32, PointerValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let ptr_type = context.ptr_type(AddressSpace::default());

        let void_type = context.void_type();
        let i64_fn_type = i64_type.fn_type(&[], false);
        module.add_function("datetime_now", i64_fn_type, Some(Linkage::External));

        let free_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("free", free_type, Some(Linkage::External));

        Self {
            context,
            module,
            builder,
            i64_type,
            ptr_type,
            locals: HashMap::new(),
        }
    }

    pub fn gen_mir(&mut self, mir: &Mir) {
        let fn_type = self.i64_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        for stmt in &mir.stmts {
            self.gen_stmt(stmt);
        }

        if !mir.stmts.iter().any(|s| matches!(s, MirStmt::Return { .. })) {
            self.builder
                .build_return(Some(&self.i64_type.const_zero()))
                .unwrap();
        }
    }

    fn gen_stmt(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr(rhs);
                let ptr = self.locals.entry(*lhs).or_insert_with(|| {
                    self.builder
                        .build_alloca(self.i64_type, &format!("loc_{lhs}"))
                        .unwrap()
                });
                self.builder.build_store(*ptr, val).unwrap();
            }

            MirStmt::Call { func, args, dest } => {
                match func.as_str() {
                    "datetime_now" => {
                        let call = self
                            .builder
                            .build_call(
                                self.module.get_function("datetime_now").unwrap(),
                                &[],
                                "tmp_dt",
                            )
                            .unwrap();

                        let val = call
                            .try_as_basic_value()
                            .expect_basic("datetime_now must return i64");

                        let ptr = self.locals.entry(*dest).or_insert_with(|| {
                            self.builder
                                .build_alloca(self.i64_type, "dt_res")
                                .unwrap()
                        });
                        self.builder.build_store(*ptr, val).unwrap();
                    }

                    "free" => {
                        let ptr_val = self.load_local(args[0]).into_pointer_value();
                        self.builder
                            .build_call(
                                self.module.get_function("free").unwrap(),
                                &[ptr_val.into()],
                                "",
                            )
                            .unwrap();
                    }

                    _ => {
                        let lhs = self.load_local(args[0]).into_int_value();
                        let rhs = args
                            .get(1)
                            .map(|&id| self.load_local(id).into_int_value())
                            .unwrap_or(self.i64_type.const_zero());

                        let result = if func.contains("add") {
                            self.builder.build_int_add(lhs, rhs, "add_tmp").unwrap()
                        } else {
                            self.builder.build_int_mul(lhs, rhs, "mul_tmp").unwrap()
                        };

                        let ptr = self.locals.entry(*dest).or_insert_with(|| {
                            self.builder
                                .build_alloca(self.i64_type, "call_res")
                                .unwrap()
                        });
                        self.builder.build_store(*ptr, result).unwrap();
                    }
                }
            }

            MirStmt::SemiringFold { op, values, result } => {
                let mut acc = self.load_local(values[0]).into_int_value();
                for &v in &values[1..] {
                    let rhs = self.load_local(v).into_int_value();
                    acc = match op {
                        SemiringOp::Add => self.builder.build_int_add(acc, rhs, "fold_add").unwrap(),
                        SemiringOp::Mul => self.builder.build_int_mul(acc, rhs, "fold_mul").unwrap(),
                    };
                }
                let ptr = self.locals.entry(*result).or_insert_with(|| {
                    self.builder
                        .build_alloca(self.i64_type, "fold_res")
                        .unwrap()
                });
                self.builder.build_store(*ptr, acc).unwrap();
            }

            MirStmt::Return { val } => {
                let v = self.load_local(*val);
                self.builder.build_return(Some(&v)).unwrap();
            }
        }
    }

    fn gen_expr(&self, expr: &MirExpr) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
        }
    }

    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = self.locals[&id];
        self.builder
            .build_load(self.i64_type, ptr, &format!("load_{id}"))
            .unwrap()
    }

    pub fn finalize_and_jit(
        &mut self,
    ) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
        self.module.verify().map_err(|e| e.to_string())?;

        let ee = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)?;

        ee.add_global_mapping(
            &self.module.get_function("datetime_now").unwrap(),
            host_datetime_now as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("free").unwrap(),
            host_free as *const () as usize,
        );

        Ok(ee)
    }
}
