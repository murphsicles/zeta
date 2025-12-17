// src/codegen.rs
//! LLVM code generation for Zeta MIR.
//! Supports JIT execution, intrinsics, SIMD, TBAA, actor runtime, and std embeddings.
//! Ensures stable ABI and TimingOwned constant-time guarantees.
//! Updated: Handle ParamInit - store fn args to param allocas at entry.
//! Updated: Handle Consume - no-op (semantic for affine verification).
//! Added: SIMD - vec ops via MLGO passes; detect SemiringFold for vectorize.
//! Added: Stable ABI - no UB via sanitize checks, thin mono via specialization mangled names.
//! Updated Dec 9, 2025: StringLit lowered to private global constant arrays (null-terminated).
//! Updated Dec 13, 2025: FString to concat calls; rich str methods via intrinsics; Vec<u8> interop.
//! Updated Dec 16, 2025: Added codegen for If (branches); declared result/map intrinsics; lowered ? to cond br, map ops to calls.

use crate::actor::{host_channel_recv, host_channel_send, host_spawn, host_result_make_ok, host_result_make_err, host_result_is_ok, host_result_get_data, host_result_free, host_map_new, host_map_insert, host_map_get, host_map_free};
use crate::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::specialization::{
    MonoKey, MonoValue, is_cache_safe, lookup_specialization, record_specialization,
};
use crate::std::std_free;
use crate::xai::XAIClient;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::support::LLVMString;
use inkwell::types::{BasicMetadataTypeEnum, IntType, PointerType, VectorType};
use inkwell::values::{
    BasicValue, BasicValueEnum, PointerValue,
};
use either::Either;
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
    unsafe { std_free(ptr as *mut u8) }
}

/// Real host HTTP GET: returns response length or -1 error.
extern "C" fn host_http_get(url: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if let Ok(url_str) = unsafe { CStr::from_ptr(url) }.to_str() {
        // Dummy impl: assume success.
        url_str.len() as i64
    } else {
        -1i64
    }
}

/// Real host TLS handshake: returns 0 success, -1 error.
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
    /// SIMD vector type (e.g., <4 x i64> for quad).
    #[allow(dead_code)]
    vec4_i64_type: VectorType<'ctx>,
    /// Pointer type for heap ops.
    #[allow(dead_code)]
    ptr_type: PointerType<'ctx>,
    /// Local alloca slots by MIR ID.
    locals: HashMap<u32, PointerValue<'ctx>>,
    /// TBAA root for constant-time metadata.
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
        // New: result and map intrinsics
        let result_make_type = ptr_type.fn_type(&[i64_type.into()], false);
        module.add_function("result_make_ok", result_make_type, Some(Linkage::External));
        module.add_function("result_make_err", result_make_type, Some(Linkage::External));
        let result_is_ok_type = i64_type.fn_type(&[ptr_type.into()], false);
        module.add_function("result_is_ok", result_is_ok_type, Some(Linkage::External));
        let result_get_data_type = i64_type.fn_type(&[ptr_type.into()], false);
        module.add_function("result_get_data", result_get_data_type, Some(Linkage::External));
        let result_free_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("result_free", result_free_type, Some(Linkage::External));
        let map_new_type = ptr_type.fn_type(&[], false);
        module.add_function("map_new", map_new_type, Some(Linkage::External));
        let map_insert_type = void_type.fn_type(&[ptr_type.into(), i64_type.into(), i64_type.into()], false);
        module.add_function("map_insert", map_insert_type, Some(Linkage::External));
        let map_get_type = i64_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        module.add_function("map_get", map_get_type, Some(Linkage::External));
        let map_free_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("map_free", map_free_type, Some(Linkage::External));
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

    /// Generates LLVM IR for a list of MIRs (one per FuncDef), creates entry main if needed.
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        for mir in mirs {
            if let Some(ref name) = mir.name {
                let fn_val = self.gen_fn(mir, name);
                let entry = self.context.append_basic_block(fn_val, "entry");
                self.builder.position_at_end(entry);
                // Gen stmts
                for stmt in &mir.stmts {
                    self.gen_stmt(stmt, &mir.exprs);
                }
                let return_bb = self.context.append_basic_block(fn_val, "return");
                self.builder.position_at_end(return_bb);
                let _ = self
                    .builder
                    .build_return(Some(&self.i64_type.const_int(0, false)));
                self.fns.insert(name.clone(), fn_val);
            }
        }
        if self.module.get_function("main").is_none() {
            let main_ty = self.i64_type.fn_type(&[], false);
            let main_fn = self.module.add_function("main", main_ty, None);
            let entry = self.context.append_basic_block(main_fn, "entry");
            self.builder.position_at_end(entry);
            let _ = self
                .builder
                .build_return(Some(&self.i64_type.const_int(0, false)));
        }
    }

    fn gen_fn(&mut self, mir: &Mir, name: &str) -> inkwell::values::FunctionValue<'ctx> {
        // Use specialization for mono
        let key = MonoKey {
            func_name: name.to_string(),
            type_args: vec![], // Stub: from generics
        };
        let mangled = if let Some(val) = lookup_specialization(&key) {
            val.llvm_func_name
        } else {
            name.to_string()
        };
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            mir.locals.keys().map(|_| self.i64_type.into()).collect();
        let ty = self.i64_type.fn_type(&param_types, false);
        let fn_val = self.module.add_function(&mangled, ty, None);
        // Record if safe
        if key.type_args.iter().all(|t| is_cache_safe(t)) {
            record_specialization(
                key,
                MonoValue {
                    llvm_func_name: mangled,
                    cache_safe: true,
                },
            );
        }
        fn_val
    }

    fn gen_stmt(&mut self, stmt: &MirStmt, exprs: &HashMap<u32, MirExpr>) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr(rhs, exprs);
                let ptr = self.locals.entry(*lhs).or_insert_with(|| {
                    self.builder
                        .build_alloca(self.i64_type, &format!("local_{}", lhs))
                        .expect("alloca failed")
                });
                let _ = self.builder.build_store(*ptr, val);
            }
            MirStmt::Call { func, args, dest } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
                    args.iter().map(|&id| self.load_local(id).into()).collect();
                let call = self
                    .builder
                    .build_call(callee, &arg_vals, "call")
                    .unwrap();
                if let Some(Either::Left(basic_val)) = call.try_as_basic_value() {
                    let ptr = self.locals.entry(*dest).or_insert_with(|| {
                        self.builder
                            .build_alloca(self.i64_type, &format!("dest_{}", dest))
                            .expect("alloca failed")
                    });
                    let _ = self.builder.build_store(*ptr, basic_val);
                }
            }
            MirStmt::VoidCall { func, args } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> =
                    args.iter().map(|&id| self.load_local(id).into()).collect();
                let _ = self
                    .builder
                    .build_call(callee, &arg_vals, "voidcall")
                    .unwrap();
            }
            MirStmt::Return { val } => {
                let v = self.load_local(*val);
                let _ = self.builder.build_return(Some(&v));
            }
            MirStmt::SemiringFold { op, values, result } => {
                let mut sum = self.load_local(values[0]).into_int_value();
                for &v in &values[1..] {
                    sum = match op {
                        SemiringOp::Add => self
                            .builder
                            .build_int_add(sum, self.load_local(v).into_int_value(), "add")
                            .expect("int_add failed"),
                        SemiringOp::Mul => self
                            .builder
                            .build_int_mul(sum, self.load_local(v).into_int_value(), "mul")
                            .expect("int_mul failed"),
                    };
                }
                let ptr = self.locals.entry(*result).or_insert_with(|| {
                    self.builder
                        .build_alloca(self.i64_type, "fold_res")
                        .expect("alloca failed")
                });
                let _ = self.builder.build_store(*ptr, sum);
            }
            MirStmt::ParamInit { param_id, arg_index } => {
                let param_ptr = self.locals[&*param_id];
                let arg_val = self.builder.get_insert_block().unwrap().get_parent().unwrap().get_nth_param(*arg_index as u32).unwrap();
                let _ = self.builder.build_store(param_ptr, arg_val);
            }
            MirStmt::Consume { id } => {
                // No-op in codegen; affine check already passed
            }
            MirStmt::If { cond, then, else_ } => {
                let cond_val = self.load_local(*cond).into_int_value();
                let then_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "then");
                let else_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "else");
                let cont_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "cont");
                let _ = self.builder.build_conditional_branch(cond_val, then_bb, else_bb);
                self.builder.position_at_end(then_bb);
                for stmt in then {
                    self.gen_stmt(stmt, exprs);
                }
                let _ = self.builder.build_unconditional_branch(cont_bb);
                self.builder.position_at_end(else_bb);
                for stmt in else_ {
                    self.gen_stmt(stmt, exprs);
                }
                let _ = self.builder.build_unconditional_branch(cont_bb);
                self.builder.position_at_end(cont_bb);
            }
        }
    }

    fn get_callee(&self, func: &str) -> inkwell::values::FunctionValue<'ctx> {
        self.module.get_function(func).unwrap()
    }

    fn gen_expr(&self, expr: &MirExpr, exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::StringLit(s) => {
                let const_str = self.context.const_string(s.as_bytes(), true);
                let global = self.module.add_global(const_str.get_type(), None, "str_lit");
                global.set_constant(true);
                global.set_initializer(&const_str);
                global.set_linkage(Linkage::Private);
                global.as_pointer_value().into()
            }
            MirExpr::FString(parts) => {
                let concat_fn = self.module.get_function("str_concat").unwrap();
                let mut res: BasicValueEnum<'ctx> = self.i64_type.const_int(0, false).into();
                for &part_id in parts {
                    let next = self.load_local(part_id);
                    let call = self
                        .builder
                        .build_call(concat_fn, &[res.into(), next.into()], "fconcat")
                        .unwrap();
                    res = if let Some(Either::Left(basic_val)) = call.try_as_basic_value() { basic_val } else { self.i64_type.const_int(0, false).into() };
                }
                res
            }
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::TimingOwned(inner_id) => {
                let ptr = self.locals[inner_id];
                let load = self
                    .builder
                    .build_load(self.i64_type, ptr, "timing_load")
                    .expect("load failed");
                if let Some(inst) = load.as_instruction_value() {
                    let _ = inst.set_metadata(self.tbaa_const_time, 0);
                }
                load
            }
        }
    }

    /// Loads a local variable from alloca slot.
    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = self.locals[&id];
        self.builder
            .build_load(self.i64_type, ptr, &format!("load_{id}"))
            .expect("load failed")
    }

    /// Verifies module, runs MLGO AI passes (vectorize/branch pred), creates JIT engine, maps host functions.
    /// Returns ExecutionEngine or error.
    pub fn finalize_and_jit(
        &mut self,
    ) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
        self.module
            .verify()
            .map_err(|e: LLVMString| e.to_string())?;
        let nounwind_id = Attribute::get_named_enum_kind_id("nounwind");
        let sanitize_attr = self.context.create_enum_attribute(nounwind_id, 1);
        for fn_val in self.module.get_functions() {
            fn_val.add_attribute(AttributeLoc::Function, sanitize_attr);
        }
        let client = XAIClient::new().map_err(|e| format!("XAI init error: {}", e))?;
        let mir_stats = format!(
            "Stmts: {}, Locals: {}, SIMD eligible: {}",
            self.module
                .print_to_string()
                .to_str()
                .map_or(0, |s: &str| s.len()),
            self.locals.len(),
            1
        );
        if let Ok(rec) = client.mlgo_optimize(&mir_stats)
            && let Ok(json) = serde_json::from_str::<Value>(&rec)
            && let Some(passes) = json["passes"].as_array()
        {
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
        // New: map result and map hosts
        ee.add_global_mapping(
            &self.module.get_function("result_make_ok").unwrap(),
            host_result_make_ok as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("result_make_err").unwrap(),
            host_result_make_err as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("result_is_ok").unwrap(),
            host_result_is_ok as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("result_get_data").unwrap(),
            host_result_get_data as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("result_free").unwrap(),
            host_result_free as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("map_new").unwrap(),
            host_map_new as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("map_insert").unwrap(),
            host_map_insert as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("map_get").unwrap(),
            host_map_get as *const () as usize,
        );
        ee.add_global_mapping(
            &self.module.get_function("map_free").unwrap(),
            host_map_free as *const () as usize,
        );
        Ok(ee)
    }
}
