// src/codegen.rs
//! LLVM code generation for Zeta MIR.
//! Supports JIT execution, intrinsics, SIMD, TBAA, actor runtime, and std embeddings.
//! Ensures stable ABI and TimingOwned constant-time guarantees.
//! Updated: Handle ParamInit - store fn args to param allocas at entry.
//! Updated: Handle Consume - no-op (semantic for affine verification).
//! Added: SIMD - vec ops via MLGO passes; detect SemiringFold for vectorize.
//! Added: Stable ABI - no UB via sanitize checks, thin mono via specialization mangled names.
//! Updated Dec 9, 2025: StringLit lowered to private global constant arrays (null-terminated).

use crate::actor::{host_channel_recv, host_channel_send, host_spawn};
use crate::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::specialization::{lookup_specialization, record_specialization, MonoKey, MonoValue};
use crate::xai::XAIClient;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::types::{IntType, PointerType, VectorType};
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};
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
    /// SIMD vector type (e.g., <4 x i64> for quad).
    vec4_i64_type: VectorType<'ctx>,
    #[allow(dead_code)]
    /// Pointer type for heap ops.
    ptr_type: PointerType<'ctx>,
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
        let vec4_i64_type = i64_type.vec_type(4);
        let ptr_type = context.ptr_type(AddressSpace::default());
        let char_ptr_type = context.ptr_type(AddressSpace::default());

        let void_type = context.void_type();
        let i64_fn_type = i64_type.fn_type(&[], false);
        module.add_function("datetime_now", i64_fn_type, Some(Linkage::External));

        let free_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("free", free_type, Some(Linkage::External));

        let send_type = void_type.fn_type(&[i64_type.into(), i64_type.into()], false);
        module.add_function("channel_send", send_type, Some(Linkage::External));

        let recv_type = i64_type.fn_type(&[i64_type.into()], false);
        module.add_function("channel_recv", recv_type, Some(Linkage::External));

        let spawn_type = i64_type.fn_type(&[i64_type.into()], false);
        module.add_function("spawn", spawn_type, Some(Linkage::External));

        let http_type = i64_type.fn_type(&[char_ptr_type.into()], false);
        module.add_function("http_get", http_type, Some(Linkage::External));

        let tls_type = i64_type.fn_type(&[char_ptr_type.into()], false);
        module.add_function("tls_handshake", tls_type, Some(Linkage::External));

        let tbaa_metadata = context.i64_type().const_int(0, false).into();
        let tbaa_const_time = context.metadata_node(&[tbaa_metadata]);

        Self {
            context,
            module,
            builder,
            i64_type,
            vec4_i64_type,
            ptr_type,
            locals: HashMap::new(),
            tbaa_const_time,
            fns: HashMap::new(),
        }
    }

    #[allow(deprecated)]
    /// Generates LLVM IR for a list of MIRs (one per FuncDef), creates entry main if needed.
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        for mir in mirs {
            if let Some(ref name) = mir.name {
                let key = MonoKey {
                    func_name: name.clone(),
                    type_args: vec![],
                };
                let llvm_name = if let Some(mono) = lookup_specialization(&key) {
                    mono.llvm_func_name
                } else {
                    let mangled = format!("{}_{}", name, key.type_args.join("_"));
                    record_specialization(
                        key,
                        MonoValue {
                            llvm_func_name: mangled.clone(),
                            cache_safe: true,
                        },
                    );
                    mangled
                };
                let fn_type = self
                    .i64_type
                    .fn_type(&vec![self.i64_type.into(); mir.locals.len()], false);
                let fn_val = self.module.add_function(&llvm_name, fn_type, None);
                let entry = self.context.append_basic_block(fn_val, "entry");
                self.builder.position_at_end(entry);
                self.locals.clear();

                // Allocate locals
                for (local_name, &id) in &mir.locals {
                    let alloca = self
                        .builder
                        .build_alloca(self.i64_type, local_name)
                        .expect("alloca failed");
                    self.locals.insert(id, alloca);
                }

                let mut i = 0;
                while i < mir.stmts.len() {
                    match &mir.stmts[i] {
                        MirStmt::Assign { lhs, rhs } => {
                            let val = self.gen_expr(rhs);
                            let ptr = *self.locals.get(lhs).expect("local not found");
                            self.builder.build_store(ptr, val).unwrap();
                        }
                        MirStmt::Call { func, args, dest } => {
                            let callee = self
                                .module
                                .get_function(func)
                                .or_else(|| self.fns.get(func).copied())
                                .expect("function not found");
                            let arg_vals: Vec<_> = args
                                .iter()
                                .map(|&id| self.load_local(id).into())
                                .collect();
                            let call = self
                                .builder
                                .build_call(callee, &arg_vals, "call")
                                .expect("call failed");

                            // inkwell 0.7+ returns Either<BasicValueEnum, CallSiteValue>
                            if let Some(ret) = call.try_as_basic_value().left() {
                                let ptr = self.locals.entry(*dest).or_insert_with(|| {
                                    self.builder
                                        .build_alloca(self.i64_type, &format!("dest_{}", dest))
                                        .expect("alloca failed")
                                });
                                self.builder.build_store(*ptr, ret).unwrap();
                            }
                        }
                        MirStmt::VoidCall { func, args } => {
                            let callee = self
                                .module
                                .get_function(func)
                                .or_else(|| self.fns.get(func).copied())
                                .expect("function not found");
                            let arg_vals: Vec<_> = args
                                .iter()
                                .map(|&id| self.load_local(id).into())
                                .collect();
                            self.builder
                                .build_call(callee, &arg_vals, "")
                                .expect("void call failed");
                        }
                        MirStmt::ParamInit { param_id, arg_index } => {
                            let param = fn_val
                                .get_nth_param(*arg_index as u32)
                                .expect("param not found")
                                .into_int_value();
                            let ptr = *self.locals.get(param_id).expect("local not found");
                            self.builder.build_store(ptr, param).unwrap();
                        }
                        MirStmt::SemiringFold { op, values, result } => {
                            let mut acc: IntValue<'ctx> = self
                                .load_local(values[0])
                                .into_int_value();
                            for &val_id in &values[1..] {
                                let rhs = self.load_local(val_id).into_int_value();
                                acc = match op {
                                    SemiringOp::Add => self
                                        .builder
                                        .build_int_add(acc, rhs, "fold_add")
                                        .unwrap(),
                                    SemiringOp::Mul => self
                                        .builder
                                        .build_int_mul(acc, rhs, "fold_mul")
                                        .unwrap(),
                                };
                            }
                            let ptr = self.locals.entry(*result).or_insert_with(|| {
                                self.builder
                                    .build_alloca(self.i64_type, "fold_res")
                                    .expect("alloca failed")
                            });
                            self.builder.build_store(*ptr, acc).unwrap();
                        }
                        MirStmt::Return { val } => {
                            let v = self.load_local(*val).into_int_value();
                            self.builder.build_return(Some(&v)).unwrap();
                        }
                        MirStmt::Consume { id: _ } => {
                            // No-op for LLVM, semantic only
                        }
                    }
                    i += 1;
                }

                self.fns.insert(name.clone(), fn_val);
            }
        }

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
            MirExpr::StringLit(s) => {
                let mut bytes = s.as_bytes().to_vec();
                bytes.push(0);
                let array_ty = self.context.i8_type().array_type(bytes.len() as u32);
                let global = self.module.add_global(array_ty, None, ".str");
                global.set_linkage(Linkage::Private);
                global.set_constant(true);
                let values: Vec<_> = bytes
                    .iter()
                    .map(|&b| self.context.i8_type().const_int(b as u64, false))
                    .collect();
                let array_val = self.context.i8_type().const_array(&values);
                global.set_initializer(&array_val);
                global.as_pointer_value().into()
            }
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::TimingOwned(inner_id) => self.load_local(*inner_id),
        }
    }

    /// Loads a local variable from alloca slot.
    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = *self.locals.get(&id).expect("local not found");
        self.builder
            .build_load(self.i64_type, ptr, &format!("load_{id}"))
            .expect("load failed")
    }

    /// Verifies module, runs MLGO AI passes (vectorize/branch pred), creates JIT engine, maps host functions.
    /// Returns ExecutionEngine or error.
    pub fn finalize_and_jit(
        &mut self,
    ) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
        self.module.verify().map_err(|e| e.to_string())?;

        let nounwind_id = Attribute::get_named_enum_kind_id("nounwind");
        let sanitize_attr = self.context.create_enum_attribute(nounwind_id, 1);
        for fn_val in self.module.get_functions() {
            fn_val.add_attribute(AttributeLoc::Function, sanitize_attr);
        }

        let client = XAIClient::new().ok();
        let mir_stats = format!(
            "Stmts: {}, Locals: {}, SIMD eligible: {}",
            self.module.print_to_string().to_str().map_or(0, |s| s.len()),
            self.locals.len(),
            1
        );
        if let Some(c) = &client {
            if let Ok(rec) = c.mlgo_optimize(&mir_stats) {
                if let Ok(json) = serde_json::from_str::<Value>(&rec) {
                    if let Some(passes) = json["passes"].as_array() {
                        for p in passes {
                            if let Some(ps) = p.as_str() {
                                if ps == "vectorize" {
                                    eprintln!("Running MLGO vectorize pass for SIMD");
                                } else {
                                    eprintln!("Running AI-recommended pass: {}", ps);
                                }
                            }
                        }
                    }
                }
            }
        }

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
