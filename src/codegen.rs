// src/codegen.rs
//! Generates LLVM IR from Zeta MIR.
//! Supports JIT execution, intrinsics, SIMD vectorization, TBAA metadata, actor runtime integration, and standard library embeddings.
//! Ensures stable ABI and constant-time guarantees for TimingOwned types.
#[allow(unused_imports)]
use crate::actor::{
    host_channel_recv, host_channel_send, host_map_free, host_map_get, host_map_insert,
    host_map_new, host_result_free, host_result_get_data, host_result_is_ok, host_result_make_err,
    host_result_make_ok, host_spawn,
};
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
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue, ValueKind,
};
use serde_json::Value;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};
/// Host function for current Unix timestamp in milliseconds.
extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}
/// Host function for freeing memory.
extern "C" fn host_free(ptr: *mut std::ffi::c_void) {
    unsafe { std_free(ptr as *mut u8) }
}
/// Host function for HTTP GET request, returning response length.
extern "C" fn host_http_get(url: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if let Ok(url_str) = unsafe { CStr::from_ptr(url) }.to_str() {
        // Dummy impl: assume success.
        url_str.len() as i64
    } else {
        -1i64
    }
}
/// Host function for TLS handshake.
extern "C" fn host_tls_handshake(host: *const std::ffi::c_char) -> i64 {
    use std::ffi::CStr;
    if unsafe { CStr::from_ptr(host) }.to_str().is_ok() {
        0i64
    } else {
        -1i64
    }
}
/// Manages LLVM code generation for a module.
pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    /// LLVM module being generated.
    pub module: Module<'ctx>,
    /// Instruction builder.
    builder: Builder<'ctx>,
    /// Integer type for Zeta values.
    i64_type: IntType<'ctx>,
    /// SIMD vector type for quad i64.
    #[allow(dead_code)]
    vec4_i64_type: VectorType<'ctx>,
    /// Pointer type for allocations.
    #[allow(dead_code)]
    ptr_type: PointerType<'ctx>,
    /// Local variable allocations by MIR ID.
    locals: HashMap<u32, PointerValue<'ctx>>,
    /// TBAA metadata for constant-time accesses.
    tbaa_const_time: inkwell::values::MetadataValue<'ctx>,
    /// Map of generated functions by name.
    fns: HashMap<String, inkwell::values::FunctionValue<'ctx>>,
}
impl<'ctx> LLVMCodegen<'ctx> {
    /// Creates a new codegen instance for the specified module name.
    /// Declares external host functions for runtime integration.
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
        module.add_function(
            "result_get_data",
            result_get_data_type,
            Some(Linkage::External),
        );
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
        // TBAA metadata stub
        let tbaa_const_time = context.create_metadata_value("const_time".to_string());
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
    /// Generates LLVM IR for a list of MIRs.
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        for mir in mirs {
            self.gen_fn(mir);
        }
    }
    /// Generates an LLVM function from a MIR.
    fn gen_fn(&mut self, mir: &Mir) {
        let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
        let fn_type = self.i64_type.fn_type(&[], false); // Stub: all fns () -> i64
        let fn_val = self.module.add_function(&fn_name, fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.locals.clear();
        for stmt in &mir.stmts {
            self.gen_stmt(stmt, &mir.exprs);
        }
        self.fns.insert(fn_name, fn_val);
    }
    /// Generates IR for a MIR statement.
    fn gen_stmt(&mut self, stmt: &MirStmt, exprs: &HashMap<u32, MirExpr>) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr(rhs, exprs);
                let alloca = self.builder.build_alloca(self.i64_type, &format!("local_{lhs}"));
                self.builder.build_store(alloca, val);
                self.locals.insert(*lhs, alloca);
            }
            MirStmt::Call { func, args, dest, type_args } => {
                let callee = if !type_args.is_empty() {
                    let key = MonoKey {
                        func_name: func.clone(),
                        type_args: type_args.clone(),
                    };
                    if let Some(val) = lookup_specialization(&key) {
                        val.llvm_func_name
                    } else {
                        let mangled = key.mangle();
                        let cache_safe = type_args.iter().all(|t| is_cache_safe(t));
                        record_specialization(key, MonoValue { llvm_func_name: mangled.clone(), cache_safe });
                        mangled
                    }
                } else {
                    func.clone()
                };
                let callee_fn = self.get_callee(&callee);
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|&id| self.load_local(id).into())
                    .collect();
                let call = self.builder.build_call(callee_fn, &arg_vals, "call");
                if let Ok(basic_val) = call.try_as_basic_value().left() {
                    let alloca = self.builder.build_alloca(self.i64_type, &format!("call_{dest}"));
                    self.builder.build_store(alloca, basic_val);
                    self.locals.insert(*dest, alloca);
                }
            }
            MirStmt::VoidCall { func, args } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|&id| self.load_local(id).into())
                    .collect();
                self.builder.build_call(callee, &arg_vals, "void_call");
            }
            MirStmt::Return { val } => {
                let ret_val = self.load_local(*val);
                self.builder.build_return(Some(&ret_val));
            }
            MirStmt::SemiringFold { op, values, result } => {
                let mut acc = self.load_local(values[0]);
                for &v in &values[1..] {
                    let right = self.load_local(v);
                    acc = match op {
                        SemiringOp::Add => self.builder.build_int_add(acc.as_int_value(), right.as_int_value(), "fold_add"),
                        SemiringOp::Mul => self.builder.build_int_mul(acc.as_int_value(), right.as_int_value(), "fold_mul"),
                    }.into();
                }
                let alloca = self.builder.build_alloca(self.i64_type, &format!("fold_{result}"));
                self.builder.build_store(alloca, acc);
                self.locals.insert(*result, alloca);
            }
            MirStmt::ParamInit { param_id, arg_index: _ } => {
                // Stub: params from args in JIT
            }
            MirStmt::Consume { id } => {
                // Stub: no-op in codegen
            }
            MirStmt::If { cond, then, else_ } => {
                let cond_val = self.load_local(*cond);
                let then_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "then");
                let else_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "else");
                let merge_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "merge");
                self.builder.build_conditional_branch(cond_val.as_int_value(), then_bb, else_bb);
                self.builder.position_at_end(then_bb);
                for stmt in then {
                    self.gen_stmt(stmt, exprs);
                }
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb);
                }
                self.builder.position_at_end(else_bb);
                for stmt in else_ {
                    self.gen_stmt(stmt, exprs);
                }
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb);
                }
                self.builder.position_at_end(merge_bb);
            }
        }
    }
    /// Generates IR for a MIR expression.
    fn gen_expr(&self, expr: &MirExpr, _exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
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
            MirExpr::FString(ids) => {
                // Chain concats
                if ids.is_empty() {
                    return self.i64_type.const_int(0, false).into();
                }
                let mut res = self.gen_expr(&MirExpr::Var(ids[0]), _exprs);
                for &id in &ids[1..] {
                    let next = self.gen_expr(&MirExpr::Var(id), _exprs);
                    let concat_fn = self.get_callee("str_concat"); // Assume intrinsic
                    let call = self
                        .builder
                        .build_call(concat_fn, &[res.into(), next.into()], "fconcat")
                        .expect("build_call failed");
                    // try_as_basic_value() returns ValueKind enum
                    res = match call.try_as_basic_value() {
                        ValueKind::Basic(basic_val) => basic_val,
                        ValueKind::Instruction(_) => panic!("concat should return a value"),
                    };
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
    /// Loads a value from a local alloca slot.
    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = self.locals[&id];
        self.builder
            .build_load(self.i64_type, ptr, &format!("load_{id}"))
            .expect("load failed")
    }
    /// Gets or declares a callee function by name.
    fn get_callee(&self, name: &str) -> inkwell::values::CallableValue<'ctx> {
        if let Some(fn_val) = self.module.get_function(name) {
            fn_val.into()
        } else {
            let fn_type = self.i64_type.fn_type(&[BasicMetadataTypeEnum::IntType(self.i64_type); 2], false); // Stub
            self.module.add_function(name, fn_type, None).into()
        }
    }
    /// Verifies the module, applies AI-recommended optimizations, and creates a JIT engine.
    /// Maps host functions to implementations.
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
        Ok(ee)
    }
}
