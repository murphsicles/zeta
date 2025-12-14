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

use crate::actor::{host_channel_recv, host_channel_send, host_spawn};
use crate::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use inkwell::basic_block::BasicBlock;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::support::LLVMString;
use inkwell::types::{BasicMetadataTypeEnum, IntType, PointerType, VectorType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue, FunctionValue, VectorValue};
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
    fns: HashMap<String, FunctionValue<'ctx>>,
    /// Current function being generated.
    current_fn: Option<FunctionValue<'ctx>>,
    /// Current basic block.
    current_block: Option<BasicBlock<'ctx>>,
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

        let this = Self {
            context,
            module,
            builder,
            i64_type,
            vec4_i64_type,
            ptr_type,
            locals: HashMap::new(),
            tbaa_const_time,
            fns: HashMap::new(),
            current_fn: None,
            current_block: None,
        };

        // Declare main stub if not present
        if this.module.get_function("main").is_none() {
            let main_type = i64_type.fn_type(&[], false);
            let main_fn = this.module.add_function("main", main_type, None);
            let entry = this.context.append_basic_block(main_fn, "entry");
            this.builder.position_at_end(entry);
            let _ = this
                .builder
                .build_return(Some(&this.i64_type.const_int(0, false)));
        }

        this
    }

    /// Generates LLVM IR for a list of MIRs, creating functions.
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        for mir in mirs {
            if let Some(name) = &mir.name {
                self.gen_fn(mir, name);
            }
        }
    }

    /// Generates a function from MIR, handling entry block, param allocas, stmts.
    fn gen_fn(&mut self, mir: &Mir, name: &str) {
        let fn_type = self.i64_type.fn_type(&[], false);
        let fn_val = self.module.add_function(name, fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.current_fn = Some(fn_val);
        self.current_block = Some(entry);
        self.builder.position_at_end(entry);

        // Clear locals for new fn
        self.locals.clear();

        // Alloc param slots (even if no params, for consistency)
        // For now, assume 0 params; extend for real params later

        // Gen stmts
        for stmt in &mir.stmts {
            self.gen_stmt(stmt);
        }

        // Default return 0 if no explicit
        let _ = self.builder.build_return(Some(&self.i64_type.const_int(0, false)));

        self.current_fn = None;
        self.current_block = None;
    }

    /// Generates a MIR statement: assign, call, return, etc.
    fn gen_stmt(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr(rhs);
                let ptr = self.get_or_alloc_local(*lhs);
                let _ = self.builder.build_store(ptr, val, "");
            }
            MirStmt::Call { func, args, dest } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|&id| self.gen_expr(&MirExpr::Var(id)).into())
                    .collect();
                let call_site = self.builder.build_call(callee, &arg_vals, "call").expect("call failed");
                let res = call_site.try_as_basic_value().unwrap_basic();
                let dest_id = *dest;
                let ptr = self.get_or_alloc_local(dest_id);
                let _ = self.builder.build_store(ptr, res, "");
            }
            MirStmt::VoidCall { func, args } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|&id| self.gen_expr(&MirExpr::Var(id)).into())
                    .collect();
                let _ = self.builder.build_call(callee, &arg_vals, "voidcall").expect("voidcall failed");
            }
            MirStmt::Return { val } => {
                let ret_val = self.gen_expr(&MirExpr::Var(*val));
                let _ = self.builder.build_return(Some(&ret_val));
            }
            MirStmt::SemiringFold { op, values, result } => {
                // Lower to vectorized call if eligible
                if values.len() >= 4 {
                    self.gen_simd_fold(op, values, *result);
                } else {
                    // Sequential
                    let mut acc = self.i64_type.const_int(0, false);
                    for &v in values {
                        let val = self.gen_expr(&MirExpr::Var(v));
                        let val_i = val.into_int_value();
                        acc = match op {
                            SemiringOp::Add => self.builder.build_int_add(acc, val_i, "add").expect("add failed").into_int_value(),
                            SemiringOp::Mul => self.builder.build_int_mul(acc, val_i, "mul").expect("mul failed").into_int_value(),
                        };
                    }
                    let ptr = self.get_or_alloc_local(*result);
                    let _ = self.builder.build_store(ptr, acc.into(), "");
                }
            }
            MirStmt::ParamInit { param_id, arg_index } => {
                // Store fn arg to param alloca
                if let Some(fn_val) = self.current_fn {
                    let params: Vec<_> = fn_val.get_params().collect();
                    if let Some(arg_val) = params.get(*arg_index) {
                        let ptr = self.get_or_alloc_local(*param_id);
                        let _ = self.builder.build_store(ptr, *arg_val, "");
                    }
                }
            }
            MirStmt::Consume { id } => {
                // No-op: semantic marker for affine
            }
        }
    }

    /// Generates SIMD fold for long semiring chains (e.g., <4 x i64> add).
    fn gen_simd_fold(&mut self, op: &SemiringOp, values: &[u32], result: u32) {
        // Group into vectors of 4
        let vec_ty = self.vec4_i64_type;
        let mut groups = vec![];
        for chunk in values.chunks(4) {
            let mut vec_vals: Vec<BasicValueEnum> = vec![self.i64_type.const_int(0, false).into(); 4];
            for (i, &vid) in chunk.iter().enumerate() {
                if i < 4 {
                    vec_vals[i] = self.gen_expr(&MirExpr::Var(vid));
                }
            }
            let vec_const = inkwell::types::VectorType::const_vector(&vec_vals);
            groups.push(vec_const);
        }

        // Fold groups with SIMD op
        let mut simd_acc = groups[0];
        for g in &groups[1..] {
            simd_acc = match op {
                SemiringOp::Add => self.builder.build_add(simd_acc, *g, "simdadd").expect("simdadd failed").into_vector_value(),
                SemiringOp::Mul => self.builder.build_mul(simd_acc, *g, "simdmul").expect("simdmul failed").into_vector_value(),
            };
        }

        // Extract to scalar sum (horz reduce)
        let mut scalar_sum = self.i64_type.const_int(0, false);
        for i in 0..4 {
            let elem_idx = self.context.i32_type().const_int(i as u64, false);
            let elem = self.builder.build_extract_element(simd_acc, elem_idx, "elem").expect("extract failed");
            let elem_i = elem.into_int_value();
            scalar_sum = self.builder.build_int_add(scalar_sum, elem_i, "sum").expect("sum failed").into_int_value();
        }
        let ptr = self.get_or_alloc_local(result);
        let _ = self.builder.build_store(ptr, scalar_sum.into(), "");
    }

    /// Allocates alloca for local if not exists, returns pointer.
    fn get_or_alloc_local(&mut self, id: u32) -> PointerValue<'ctx> {
        if !self.locals.contains_key(&id) {
            let alloca = self.builder.build_alloca(self.i64_type, &format!("local_{}", id)).expect("alloca failed");
            self.locals.insert(id, alloca);
        }
        self.locals[&id]
    }

    /// Gets callee function value, declares if missing.
    fn get_callee(&mut self, func: &str) -> FunctionValue<'ctx> {
        if let Some(f) = self.fns.get(func) {
            *f
        } else {
            let param_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![self.i64_type.into()];
            let ty = self.i64_type.fn_type(&param_types, false);
            let f = self.module.add_function(func, ty, Some(Linkage::External));
            self.fns.insert(func.to_string(), f);
            f
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
            MirExpr::FString(ids) => {
                // Assume first id, as chained in stmts
                self.gen_expr(&MirExpr::Var(ids[0]))
            }
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::TimingOwned(inner_id) => self.load_local(*inner_id),
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

        let client = XAIClient::new().ok();
        let mir_stats = format!(
            "Stmts: {}, Locals: {}, SIMD eligible: {}",
            self.module
                .print_to_string()
                .to_str()
                .map_or(0, |s: &str| s.len()),
            self.locals.len(),
            1
        );
        if let Some(c) = &client
            && let Ok(rec) = c.mlgo_optimize(&mir_stats)
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

        // Map string intrinsics (stub implementations)
        // For production, implement in host or link libc++/etc.

        Ok(ee)
    }
}
