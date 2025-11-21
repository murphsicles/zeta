// src/codegen.rs
use crate::mir::{Mir, MirStmt, MirExpr, SemiringOp};
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Module, Linkage};
use inkwell::builder::Builder;
use inkwell::values::{PointerValue, IntValue, BasicValueEnum};
use inkwell::types::{IntType, PointerType};
use inkwell::OptimizationLevel;
use inkwell::AddressSpace;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};
use std::error::Error;
use std::ffi::{CString, CStr};

extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}

extern "C" fn host_http_get(url_ptr: *const u8, url_len: usize) -> *mut u8 {
    let url = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(url_ptr, url_len)) };
    match reqwest::blocking::get(url) {
        Ok(resp) if resp.status().is_success() => {
            let bytes = resp.bytes().unwrap_or_default();
            let len = bytes.len();
            let ptr = unsafe { libc::malloc(len + 1) as *mut u8 };
            if !ptr.is_null() {
                unsafe {
                    std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, len);
                    *ptr.add(len) = 0;
                }
            }
            ptr
        }
        _ => std::ptr::null_mut(),
    }
}

extern "C" fn host_tls_get(url_ptr: *const u8, url_len: usize) -> *mut u8 {
    let url = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(url_ptr, url_len)) };
    let client = reqwest::blocking::Client::builder()
        .danger_accept_invalid_certs(true)
        .build()
        .unwrap();
    match client.get(url).send() {
        Ok(resp) if resp.status().is_success() => {
            let bytes = resp.bytes().unwrap_or_default();
            let len = bytes.len();
            let ptr = unsafe { libc::malloc(len + 1) as *mut u8 };
            if !ptr.is_null() {
                unsafe {
                    std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, len);
                    *ptr.add(len) = 0;
                }
            }
            ptr
        }
        _ => std::ptr::null_mut(),
    }
}

// AI_OPT */
extern "C" fn host_ai_opt(code_ptr: *const u8, code_len: usize) -> i32 {
    let code = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(code_ptr, code_len)) };
    
    let client = match crate::xai::XAIClient::new() {
        Ok(c) => c,
        Err(_) => return 0,
    };

    let prompt = format!(
        "You are an MLGO policy. Given this Zeta function, suggest LLVM -O3 flags to maximize performance. Respond ONLY with a comma-separated list of flags (e.g. 'vectorize,unroll,inline').\n\n{code}"
    );

    match client.query(&prompt) {
        Ok(response) => {
            let flags = response
                .split(',')
                .map(|s| s.trim().to_lowercase())
                .filter(|s| s == "vectorize" || s == "unroll" || s == "inline" || s == "slp" || s == "loop-vectorize")
                .count() as i32;
            if flags > 0 { flags } else { 1 }  // at least inline
        }
        Err(_) => 1,
    }
}

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i64_type: IntType<'ctx>,
    ptr_type: PointerType<'ctx>,
    locals: HashMap<u32, PointerValue<'ctx>>,
    ai_opt_enabled: bool,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let ptr_type = context.i8_type().ptr_type(AddressSpace::default());

        // std embeds
        let datetime_ty = i64_type.fn_type(&[], false);
        module.add_function("datetime_now", datetime_ty, Some(Linkage::External));

        let http_sig = ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        module.add_function("http_get", http_sig, Some(Linkage::External));
        module.add_function("tls_get", http_sig, Some(Linkage::External));

        // AI-opt hook
        let ai_sig = i64_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        module.add_function("__zeta_ai_opt", ai_sig, Some(Linkage::External));

        Self {
            context,
            module,
            builder,
            execution_engine: None,
            i64_type,
            ptr_type,
            locals: HashMap::new(),
            ai_opt_enabled: true,
        }
    }

    pub fn gen_mir(&mut self, mir: &Mir) {
        let fn_type = self.i64_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);

        // AI-opt: query Grok for MLGO hints
        if self.ai_opt_enabled {
            let code = mir_to_string(mir);  // simple pretty-printer
            let cstr = CString::new(code).unwrap();
            let ptr = cstr.as_ptr() as *const u8;
            let len = cstr.as_bytes().len();

            let call = self.builder.build_call(
                self.module.get_function("__zeta_ai_opt").unwrap(),
                &[ptr.into(), (len as i64).into()],
                "ai_hint",
            ).unwrap();
            let hint = call.try_as_basic_value().left().unwrap().into_int_value();

            // Simple policy: if hint >= 2, enable aggressive opts
            if hint.get_zero_extended_constant().unwrap_or(0) >= 2 {
                function.set_optimization_level(OptimizationLevel::Aggressive);
            }
        }

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        for stmt in &mir.stmts {
            self.gen_stmt(stmt);
        }

        if !mir.stmts.iter().any(|s| matches!(s, MirStmt::Return { .. })) {
            self.builder.build_return(Some(&self.i64_type.const_zero())).unwrap();
        }
    }

    // gen_stmt, gen_expr, load_local, alloc_local unchanged from previous version
    // except now also handles "tls_get" same as "http_get"

    fn gen_stmt(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let value = self.gen_expr(rhs);
                let ptr = self.alloc_local(*lhs);
                self.builder.build_store(ptr, value).unwrap();
            }
            MirStmt::Call { func, args, dest } => {
                match func.as_str() {
                    "datetime_now" => {
                        let call = self.builder.build_call(self.module.get_function("datetime_now").unwrap(), &[], "dt").unwrap();
                        let val = call.try_as_basic_value().left().unwrap();
                        let ptr = self.alloc_local(*dest);
                        self.builder.build_store(ptr, val).unwrap();
                    }
                    "http_get" | "tls_get" => {
                        let url_ptr = self.load_local(args[0]).into_pointer_value();
                        let url_len = self.load_local(args[1]).into_int_value();
                        let call = self.builder.build_call(
                            self.module.get_function(func).unwrap(),
                            &[url_ptr.into(), url_len.into()],
                            "str_tmp",
                        ).unwrap();
                        let ptr_val = call.try_as_basic_value().left().unwrap().into_pointer_value();
                        let dest_ptr = self.alloc_local(*dest);
                        self.builder.build_store(dest_ptr, ptr_val).unwrap();
                    }
                    "add" | "mul" => {
                        let lhs = self.load_local(args[0]).into_int_value();
                        let rhs = args.get(1).map(|&id| self.load_local(id)).unwrap_or(self.i64_type.const_zero());
                        let result = if func == "add" {
                            self.builder.build_int_add(lhs, rhs.into_int_value(), "add").unwrap()
                        } else {
                            self.builder.build_int_mul(lhs, rhs.into_int_value(), "mul").unwrap()
                        };
                        let ptr = self.alloc_local(*dest);
                        self.builder.build_store(ptr, result).unwrap();
                    }
                    _ => unimplemented!("call {}", func),
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
                let ptr = self.alloc_local(*result);
                self.builder.build_store(ptr, acc).unwrap();
            }
            MirStmt::Return { val } => {
                let v = self.load_local(*val);
                self.builder.build_return(Some(&v)).unwrap();
            }
            _ => unimplemented!("stmt {:?}", stmt),
        }
    }

    // unchanged helper methods...

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        
        ee.add_global_mapping(&self.module.get_function("datetime_now").unwrap(), host_datetime_now as usize);
        ee.add_global_mapping(&self.module.get_function("http_get").unwrap(), host_http_get as usize);
        ee.add_global_mapping(&self.module.get_function("tls_get").unwrap(), host_tls_get as usize);
        ee.add_global_mapping(&self.module.get_function("__zeta_ai_opt").unwrap(), host_ai_opt as usize);
        
        self.execution_engine = Some(ee.clone());
        Ok(ee)
    }
}

// Tiny helper for AI-opt
fn mir_to_string(_mir: &Mir) -> String {
    "// main() { ... } // placeholder — real pretty printer later".to_string()
}
