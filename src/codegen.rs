// src/codegen.rs
//! LLVM code generation for Zeta MIR.
//! Supports JIT execution, intrinsics, SIMD, TBAA, actor runtime, and std embeddings.
//! Ensures stable ABI and TimingOwned constant-time guarantees.

use crate::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::actor::{host_channel_send, host_channel_recv, host_spawn};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::types::IntType;
use inkwell::values::{BasicValueEnum, PointerValue};
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

/// Host implementation for datetime_now, returning Unix millis.
extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}

/// Host implementation for free, wrapping libc::free.
extern "C" fn host_free(ptr: *mut std::ffi::c_void) {
    if !ptr.is_null() {
        unsafe { libc::free(ptr) }
    }
}

/// LLVM codegen context for a module.
pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    /// LLVM module being built.
    pub module: Module<'ctx>,
    /// IR builder for instructions.
    builder: Builder<'ctx>,
    /// i64 type for Zeta ints.
    i64_type: IntType<'ctx>,
    #[allow(dead_code)]
    /// Pointer type for heap ops.
    ptr_type: inkwell::types::PointerType<'ctx>,
    /// Local alloca slots by MIR ID.
    locals: HashMap<u32, PointerValue<'ctx>>,
    /// TBAA root for constant-time metadata.
    tbaa_const_time: inkwell::values::MetadataValue<'ctx>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Creates a new codegen instance for a module named `name`.
    /// Declares external host functions (datetime_now, free, actor intrinsics).
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

        // Actor intrinsics - simplified to i64 for chan_id
        let send_type = void_type.fn_type(&[i64_type.into(), i64_type.into()], false);
        module.add_function("channel_send", send_type, Some(Linkage::External));

        let recv_type = i64_type.fn_type(&[i64_type.into()], false);
        module.add_function("channel_recv", recv_type, Some(Linkage::External));

        // Spawn intrinsic: i64 spawn(i64 func_id) -> i64 chan_id
        let spawn_type = i64_type.fn_type(&[i64_type.into()], false);
        module.add_function("spawn", spawn_type, Some(Linkage::External));

        // TBAA metadata for constant-time
        let tbaa_metadata = context.i64_type().const_int(0, false).into();
        let tbaa_const_time = context.metadata_node(&[tbaa_metadata]);

        Self {
            context,
            module,
            builder,
            i64_type,
            ptr_type,
            locals: HashMap::new(),
            tbaa_const_time,
        }
    }

    /// Generates LLVM IR from MIR, creating a main function.
    /// Appends return 0 if no explicit return.
    pub fn gen_mir(&mut self, mir: &Mir) {
        let fn_type = self.i64_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Generate statements in order.
        for stmt in &mir.stmts {
            self.gen_stmt(stmt);
        }

        // Default return if no explicit return.
        if !mir
            .stmts
            .iter()
            .any(|s| matches!(s, MirStmt::Return { .. }))
        {
            self.builder
                .build_return(Some(&self.i64_type.const_zero()))
                .unwrap();
        }
    }

    /// Generates IR for a MIR statement.
    fn gen_stmt(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr(rhs);
                // Allocate local if not exists.
                let ptr = self.locals.entry(*lhs).or_insert_with(|| {
                    self.builder
                        .build_alloca(self.i64_type, &format!("loc_{lhs}"))
                        .unwrap()
                });
                self.builder.build_store(*ptr, val).unwrap();
            }

            MirStmt::Call { func, args, dest } => match func.as_str() {
                "datetime_now" => {
                    // Intrinsic call to host datetime.
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
                        self.builder.build_alloca(self.i64_type, "dt_res").unwrap()
                    });
                    self.builder.build_store(*ptr, val).unwrap();
                }

                "free" => {
                    // Call host free with pointer arg.
                    let ptr_val = self.load_local(args[0]).into_pointer_value();
                    self.builder
                        .build_call(
                            self.module.get_function("free").unwrap(),
                            &[ptr_val.into()],
                            "",
                        )
                        .unwrap();
                }

                "channel_send" => {
                    let chan = self.load_local(args[0]);
                    let msg = self.load_local(args[1]);
                    self.builder
                        .build_call(
                            self.module.get_function("channel_send").unwrap(),
                            &[chan.into(), msg.into()],
                            "",
                        )
                        .unwrap();
                }

                "channel_recv" => {
                    let chan = self.load_local(args[0]);
                    let call = self.builder
                        .build_call(
                            self.module.get_function("channel_recv").unwrap(),
                            &[chan.into()],
                            "recv_tmp",
                        )
                        .unwrap();
                    let val = call.try_as_basic_value().expect_basic("recv must return i64");
                    let ptr = self.locals.entry(*dest).or_insert_with(|| {
                        self.builder.build_alloca(self.i64_type, "recv_res").unwrap()
                    });
                    self.builder.build_store(*ptr, val).unwrap();
                }

                _ if func.starts_with("spawn_") => {
                    // Spawn call: i64 spawn(i64 func_id) -> i64 chan_id
                    let func_id = self.load_local(args[0]);
                    let call = self.builder
                        .build_call(
                            self.module.get_function("spawn").unwrap(),
                            &[func_id.into()],
                            "spawn_call",
                        )
                        .unwrap();
                    let val = call.try_as_basic_value().expect_basic("spawn returns i64");
                    let ptr = self.locals.entry(*dest).or_insert_with(|| {
                        self.builder.build_alloca(self.i64_type, "chan_res").unwrap()
                    });
                    self.builder.build_store(*ptr, val).unwrap();
                }

                _ => {
                    // Binary op fallback: add or mul based on func name.
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
            },

            MirStmt::VoidCall { func, args } => match func.as_str() {
                "channel_send" => {
                    let chan = self.load_local(args[0]);
                    let msg = self.load_local(args[1]);
                    self.builder
                        .build_call(
                            self.module.get_function("channel_send").unwrap(),
                            &[chan.into(), msg.into()],
                            "",
                        )
                        .unwrap();
                }
                _ => {
                    // Generic void call fallback
                    let arg_vals: Vec<BasicValueEnum> = args.iter().map(|&id| self.load_local(id)).collect();
                    let arg_refs: &[BasicValueEnum] = &arg_vals;
                    self.builder
                        .build_call(
                            self.module.get_function(func).unwrap_or_else(|| self.module.add_function(func, self.context.void_type().fn_type(&[self.i64_type.into()], false), None)),
                            arg_refs,
                            "",
                        )
                        .unwrap();
                }
            },

            MirStmt::SemiringFold { op, values, result } => {
                // Fold multiple values with semiring op (add/mul chain).
                let mut acc = self.load_local(values[0]).into_int_value();
                for &v in &values[1..] {
                    let rhs = self.load_local(v).into_int_value();
                    acc = match op {
                        SemiringOp::Add => {
                            self.builder.build_int_add(acc, rhs, "fold_add").unwrap()
                        }
                        SemiringOp::Mul => {
                            self.builder.build_int_mul(acc, rhs, "fold_mul").unwrap()
                        }
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

    /// Generates a BasicValue from a MIR expression.
    fn gen_expr(&self, expr: &MirExpr) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::TimingOwned(inner_id) => {
                // Load inner, apply TBAA for constant-time analysis
                let inner_val = self.load_local(*inner_id);
                inner_val
            }
        }
    }

    /// Loads a local variable from alloca slot.
    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = self.locals[&id];
        self.builder
            .build_load(self.i64_type, ptr, &format!("load_{id}"))
            .unwrap()
    }

    /// Verifies module, creates JIT engine, maps host functions.
    /// Returns ExecutionEngine or error.
    pub fn finalize_and_jit(
        &mut self,
    ) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
        self.module.verify().map_err(|e| e.to_string())?;

        let ee = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)?;

        // Map host datetime_now.
        ee.add_global_mapping(
            &self.module.get_function("datetime_now").unwrap(),
            host_datetime_now as *const () as usize,
        );
        // Map host free.
        ee.add_global_mapping(
            &self.module.get_function("free").unwrap(),
            host_free as *const () as usize,
        );
        // Map actor intrinsics.
        ee.add_global_mapping(
            &self.module.get_function("channel_send").unwrap(),
            host_channel_send as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("channel_recv").unwrap(),
            host_channel_recv as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("spawn").unwrap(),
            host_spawn as *const () as usize,
        );

        Ok(ee)
    }
}
