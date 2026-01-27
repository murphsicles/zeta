
// src/backend/codegen/jit.rs
use super::codegen::LLVMCodegen;
use crate::runtime::actor::channel::{host_channel_recv, host_channel_send};
use crate::runtime::actor::map::{host_map_get, host_map_insert, host_map_new};
use crate::runtime::actor::result::{host_result_get_data, host_result_is_ok};
use crate::runtime::actor::scheduler::host_spawn;
use crate::runtime::host::{
    host_datetime_now, host_free, host_http_get, host_str_concat, host_str_contains,
    host_str_ends_with, host_str_len, host_str_replace, host_str_starts_with,
    host_str_to_lowercase, host_str_to_uppercase, host_str_trim, host_tls_handshake,
};
use crate::runtime::xai::XAIClient;
use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use serde_json::Value;
use std::boxed::Box;
use std::error::Error;
impl<'ctx> LLVMCodegen<'ctx> {
    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify()?;
        Target::initialize_native(&InitializationConfig::default())?;
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)?;
        let target_machine = target
            .create_target_machine(
                &target_triple,
                &TargetMachine::get_host_cpu_name().to_string(),
                &TargetMachine::get_host_cpu_features().to_string(),
                OptimizationLevel::Aggressive,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .ok_or("Failed to create target machine")?;
        self.module.set_triple(&target_triple);
        self.module
            .set_data_layout(&target_machine.get_target_data().get_data_layout());
        let mut stmt_count = 0;
        let mut local_count = 0;
        let mut simd_eligible = 0;
        for func in self.module.get_functions() {
            for bb in func.get_basic_blocks() {
                stmt_count += bb.get_instructions().count() as u32;
                for inst in bb.get_instructions() {
                    if matches!(
                        inst.get_opcode(),
                        inkwell::values::InstructionOpcode::Add
                            | inkwell::values::InstructionOpcode::Mul
                    ) {
                        simd_eligible += 1;
                    }
                }
            }
            local_count += self.locals.len();
        }
        let mir_stats = format!(
            "Functions: {}, Instructions: {}, Locals: {}, SIMD-eligible ops: {}",
            self.module.get_functions().count(),
            stmt_count,
            local_count,
            simd_eligible
        );
        let client = XAIClient::new().map_err(|e| format!("XAI init error: {}", e))?;
        let rec = client.mlgo_optimize(&mir_stats)?;
        let json: Value = serde_json::from_str(&rec).unwrap_or(Value::Null);
        let passes = json["passes"].as_array().cloned().unwrap_or_default();
        for pass in &passes {
            if let Some(name) = pass.as_str() {
                eprintln!("MLGO-recommended pass: {}", name);
            }
        }
        let fpm = PassManager::create(&self.module);
        fpm.initialize();
        for func in self.module.get_functions() {
            fpm.run_on(&func);
        }
        let mpm = PassManager::create(());
        mpm.run_on(&self.module);
        let ee = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        if let Some(f) = self.module.get_function("datetime_now") {
            ee.add_global_mapping(&f, host_datetime_now as *const () as usize);
        }
        if let Some(f) = self.module.get_function("free") {
            ee.add_global_mapping(&f, host_free as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_concat") {
            ee.add_global_mapping(&f, host_str_concat as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_to_lowercase") {
            ee.add_global_mapping(&f, host_str_to_lowercase as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_to_uppercase") {
            ee.add_global_mapping(&f, host_str_to_uppercase as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_len") {
            ee.add_global_mapping(&f, host_str_len as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_starts_with") {
            ee.add_global_mapping(&f, host_str_starts_with as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_ends_with") {
            ee.add_global_mapping(&f, host_str_ends_with as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_contains") {
            ee.add_global_mapping(&f, host_str_contains as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_trim") {
            ee.add_global_mapping(&f, host_str_trim as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_str_replace") {
            ee.add_global_mapping(&f, host_str_replace as *const () as usize);
        }
        if let Some(f) = self.module.get_function("channel_send") {
            ee.add_global_mapping(&f, host_channel_send as *const () as usize);
        }
        if let Some(f) = self.module.get_function("channel_recv") {
            ee.add_global_mapping(&f, host_channel_recv as *const () as usize);
        }
        if let Some(f) = self.module.get_function("spawn") {
            ee.add_global_mapping(&f, host_spawn as *const () as usize);
        }
        if let Some(f) = self.module.get_function("http_get") {
            ee.add_global_mapping(&f, host_http_get as *const () as usize);
        }
        if let Some(f) = self.module.get_function("tls_handshake") {
            ee.add_global_mapping(&f, host_tls_handshake as *const () as usize);
        }
        if let Some(f) = self.module.get_function("result_is_ok") {
            ee.add_global_mapping(&f, host_result_is_ok as *const () as usize);
        }
        if let Some(f) = self.module.get_function("result_get_data") {
            ee.add_global_mapping(&f, host_result_get_data as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_map_new") {
            ee.add_global_mapping(&f, host_map_new as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_map_insert") {
            ee.add_global_mapping(&f, host_map_insert as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_map_get") {
            ee.add_global_mapping(&f, host_map_get as *const () as usize);
        }
        Ok(ee)
    }
}
// New: JIT from IR string for self-hosted compiler
pub fn host_llvm_jit_from_ir(ir: String) -> Result<ExecutionEngine<'static>, Box<dyn Error>> {
    let context_box = Box::new(Context::create());
    let context = Box::leak(context_box);
    let memory_buffer = MemoryBuffer::create_from_memory_range_copy(ir.as_bytes(), "zeta_ir");
    let module = context.create_module_from_ir(memory_buffer)?;
    Target::initialize_native(&InitializationConfig::default())?;
    let target_triple = TargetMachine::get_default_triple();
    module.set_triple(&target_triple);
    let fpm = PassManager::create(&module);
    fpm.initialize();
    for func in module.get_functions() {
        fpm.run_on(&func);
    }
    let mpm = PassManager::create(());
    mpm.run_on(&module);
    let ee = module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
    if let Some(f) = module.get_function("host_datetime_now") {
        ee.add_global_mapping(&f, host_datetime_now as *const () as usize);
    }
    if let Some(f) = module.get_function("free") {
        ee.add_global_mapping(&f, host_free as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_concat") {
        ee.add_global_mapping(&f, host_str_concat as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_to_lowercase") {
        ee.add_global_mapping(&f, host_str_to_lowercase as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_to_uppercase") {
        ee.add_global_mapping(&f, host_str_to_uppercase as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_len") {
        ee.add_global_mapping(&f, host_str_len as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_starts_with") {
        ee.add_global_mapping(&f, host_str_starts_with as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_ends_with") {
        ee.add_global_mapping(&f, host_str_ends_with as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_contains") {
        ee.add_global_mapping(&f, host_str_contains as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_trim") {
        ee.add_global_mapping(&f, host_str_trim as *const () as usize);
    }
    if let Some(f) = module.get_function("host_str_replace") {
        ee.add_global_mapping(&f, host_str_replace as *const () as usize);
    }
    if let Some(f) = module.get_function("host_channel_send") {
        ee.add_global_mapping(&f, host_channel_send as *const () as usize);
    }
    if let Some(f) = module.get_function("host_channel_recv") {
        ee.add_global_mapping(&f, host_channel_recv as *const () as usize);
    }
    if let Some(f) = module.get_function("host_spawn") {
        ee.add_global_mapping(&f, host_spawn as *const () as usize);
    }
    if let Some(f) = module.get_function("host_http_get") {
        ee.add_global_mapping(&f, host_http_get as *const () as usize);
    }
    if let Some(f) = module.get_function("host_tls_handshake") {
        ee.add_global_mapping(&f, host_tls_handshake as *const () as usize);
    }
    if let Some(f) = module.get_function("host_result_is_ok") {
        ee.add_global_mapping(&f, host_result_is_ok as *const () as usize);
    }
    if let Some(f) = module.get_function("host_result_get_data") {
        ee.add_global_mapping(&f, host_result_get_data as *const () as usize);
    }
    if let Some(f) = module.get_function("host_map_new") {
        ee.add_global_mapping(&f, host_map_new as *const () as usize);
    }
    if let Some(f) = module.get_function("host_map_insert") {
        ee.add_global_mapping(&f, host_map_insert as *const () as usize);
    }
    if let Some(f) = module.get_function("host_map_get") {
        ee.add_global_mapping(&f, host_map_get as *const () as usize);
    }
    Ok(ee)
}
pub fn host_llvm_get_main(engine_ptr: i64) -> i64 {
    if engine_ptr == 0 {
        return 0;
    }
    let ee: &ExecutionEngine = unsafe { &*(engine_ptr as *const ExecutionEngine) };
    match ee.get_function_address("main") {
        Ok(addr) => addr as i64,
        Err(_) => 0,
    }
}
