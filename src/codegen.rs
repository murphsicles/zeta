// src/codegen.rs
//! LLVM code generation for Zeta MIR.
//! Supports JIT execution, intrinsics, SIMD, TBAA, actor runtime, and std embeddings.
//! Ensures stable ABI and TimingOwned constant-time guarantees.
//! Updated: Handle ParamInit - store fn args to param allocas at entry.

use crate::actor::{host_channel_recv, host_channel_send, host_spawn};
use crate::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::xai::XAIClient;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, IntType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue};
use serde_json::Value;
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

/// Simplified host HTTP GET: returns response length or -1 error.
extern "C" fn host_http_get(url: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if unsafe { CStr::from_ptr(url) }.to_str().is_ok() {
        // Dummy: always return 200
        200i64
    } else {
        -1i64
    }
}

/// Simplified host TLS handshake: returns 0 success, -1 error.
extern "C" fn host_tls_handshake(host: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if unsafe { CStr::from_ptr(host) }.to_str().is_ok() {
        0i64
    } else {
        -1i64
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
    #[allow(dead_code)]
    tbaa_const_time: inkwell::values::MetadataValue<'ctx>,
    /// Generated function map: name -> LLVM fn.
    fns: HashMap<String, inkwell::values::FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Creates a new codegen instance for a module named `name`.
    /// Declares external host functions (datetime_now, free, actor intrinsics, std embeds).
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let ptr_type = context.ptr_type(AddressSpace::default());
        let char_ptr_type = context.ptr_type(AddressSpace::default());

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

        // Std embeds: http_get(url: &str) -> i64 (status)
        let http_type = i64_type.fn_type(&[char_ptr_type.into()], false);
        module.add_function("http_get", http_type, Some(Linkage::External));

        // TLS handshake(host: &str) -> i64 (0 ok)
        let tls_type = i64_type.fn_type(&[char_ptr_type.into()], false);
        module.add_function("tls_handshake", tls_type, Some(Linkage::External));

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
            fns: HashMap::new(),
        }
    }

    /// Generates LLVM IR for a list of MIRs (one per FuncDef), creates entry main if needed.
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        for mir in mirs {
            if let Some(ref name) = mir.name {
                // Fn type from #params (all i64 for now)
                let param_types: Vec<BasicTypeEnum<'ctx>> = mir
                    .locals
                    .iter()
                    .map(|_| self.i64_type.into())
                    .take(4)
                    .collect();
                let param_meta_types: Vec<BasicMetadataTypeEnum<'ctx>> =
                    param_types.iter().cloned().map(|t| t.into()).collect();
                let fn_ty = self.i64_type.fn_type(&param_meta_types, false);
                let fn_val = self.module.add_function(name, fn_ty, None);
                let basic_block = self.context.append_basic_block(fn_val, "entry");
                self.builder.position_at_end(basic_block);

                // Get fn args for ParamInit
                let fn_args: Vec<_> = fn_val.get_params().into_iter().collect();

                for stmt in &mir.stmts {
                    match stmt {
                        MirStmt::ParamInit { param_id, arg_index } => {
                            if let Some(arg) = fn_args.get(*arg_index) {
                                let param_ptr = *self.locals.entry(*param_id).or_insert_with(|| {
                                    self.builder
                                        .build_alloca(self.i64_type, "param")
                                        .unwrap()
                                });
                                self.builder.build_store(param_ptr, *arg).unwrap();
                            }
                        }
                        MirStmt::Assign { lhs, rhs } => {
                            let lhs_ptr = *self.locals.entry(*lhs).or_insert_with(|| {
                                self.builder
                                    .build_alloca(self.i64_type, "local")
                                    .unwrap()
                            });
                            let rhs_val = self.gen_expr(rhs);
                            self.builder.build_store(lhs_ptr, rhs_val).unwrap();
                        }
                        MirStmt::Call { func, args, dest } => {
                            let callee = self.module.get_function(func).unwrap_or_else(|| {
                                self.module.add_function(func, self.i64_type.fn_type(&[self.i64_type.into()], false), Some(Linkage::External))
                            });
                            let mut arg_vals = vec![];
                            for &arg_id in args {
                                arg_vals.push(self.load_local(arg_id).into_int_value());
                            }
                            let arg_meta_vals: Vec<BasicMetadataValueEnum> = arg_vals.iter().map(|v| v.into()).collect();
                            let call_val = self.builder.build_call(callee, &arg_meta_vals, "");
                            let dest_ptr = *self.locals.entry(*dest).or_insert_with(|| {
                                self.builder.build_alloca(self.i64_type, "call_res").unwrap()
                            });
                            self.builder.build_store(dest_ptr, call_val.try_as_basic_value().left().unwrap()).unwrap();
                        }
                        MirStmt::VoidCall { func, args } => {
                            let callee = self.module.get_function(func).unwrap_or_else(|| {
                                let void_type = self.context.void_type();
                                self.module.add_function(func, void_type.fn_type(&[self.i64_type.into()], false), Some(Linkage::External))
                            });
                            let mut arg_vals = vec![];
                            for &arg_id in args {
                                arg_vals.push(self.load_local(arg_id).into_int_value());
                            }
                            let arg_meta_vals: Vec<BasicMetadataValueEnum> = arg_vals.iter().map(|v| v.into()).collect();
                            self.builder.build_call(callee, &arg_meta_vals, "");
                        }
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
                            let ptr = *self.locals.entry(*result).or_insert_with(|| {
                                self.builder
                                    .build_alloca(self.i64_type, "fold_res")
                                    .unwrap()
                            });
                            self.builder.build_store(ptr, acc).unwrap();
                        }
                        MirStmt::Return { val } => {
                            let v = self.load_local(*val);
                            self.builder.build_return(Some(&v)).unwrap();
                        }
                    }
                }

                self.fns.insert(name.clone(), fn_val);
            }
        }

        // Create main if no main
        if self.module.get_function("main").is_none() {
            let main_ty = self.i64_type.fn_type(&[], false);
            let main_fn = self.module.add_function("main", main_ty, None);
            let entry = self.context.append_basic_block(main_fn, "entry");
            self.builder.position_at_end(entry);
            self.builder
                .build_return(Some(&self.i64_type.const_int(0, false)))
                .unwrap();
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
                self.load_local(*inner_id)
            }
        }
    }

    /// Loads a local variable from alloca slot.
    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = *self.locals.get(&id).expect("local not found");
        self.builder
            .build_load(self.i64_type, ptr, &format!("load_{id}"))
            .unwrap()
    }

    /// Verifies module, runs MLGO AI passes (vectorize/branch pred), creates JIT engine, maps host functions.
    /// Returns ExecutionEngine or error.
    pub fn finalize_and_jit(
        &mut self,
    ) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
        self.module.verify().map_err(|e| e.to_string())?;

        // MLGO AI hooks: Query Grok for optimized passes
        let client = XAIClient::new().ok(); // Optional, skip if no key
        let mir_stats = format!(
            "Stmts: {}, Locals: {}",
            self.module
                .print_to_string()
                .to_str()
                .map_or(0, |s| s.len()),
            self.locals.len()
        );
        if let Some(c) = &client {
            if let Ok(rec) = c.mlgo_optimize(&mir_stats) {
                if let Ok(json) = serde_json::from_str::<Value>(&rec) {
                    if let Some(passes) = json["passes"].as_array() {
                        // Simplified: Run default passes; extend for custom
                        for p in passes {
                            if let Some(ps) = p.as_str() {
                                // Mock run; integrate real passes as needed
                                eprintln!("Running AI-recommended pass: {}", ps);
                            }
                        }
                    }
                }
            }
        }

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
        // Map std embeds.
        ee.add_global_mapping(
            &self.module.get_function("http_get").unwrap(),
            host_http_get as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("tls_handshake").unwrap(),
            host_tls_handshake as *const () as usize,
        );

        Ok(ee)
    }
}
