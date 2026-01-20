// src/backend/codegen/jit.rs
use super::codegen::LLVMCodegen;
use crate::runtime::actor::channel::{host_channel_recv, host_channel_send};
use crate::runtime::actor::map::{host_map_free, host_map_get, host_map_insert, host_map_new};
use crate::runtime::actor::result::{host_result_free, host_result_get_data, host_result_is_ok};
use crate::runtime::actor::scheduler::host_spawn;
use crate::runtime::host::{
    host_datetime_now, host_free, host_http_get, host_str_concat, host_str_contains,
    host_str_ends_with, host_str_len, host_str_replace, host_str_starts_with,
    host_str_to_lowercase, host_str_to_uppercase, host_str_trim, host_tls_handshake,
};
use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use std::sync::OnceLock;

static GLOBAL_ENGINE: OnceLock<ExecutionEngine<'static>> = OnceLock::new();

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
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

        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;

        // Global mappings for all host functions
        let module = &self.module;

        ee.add_global_mapping(
            &module.get_function("datetime_now").expect("missing datetime_now"),
            host_datetime_now as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("free").expect("missing free"),
            host_free as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("http_get").expect("missing http_get"),
            host_http_get as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("tls_handshake").expect("missing tls_handshake"),
            host_tls_handshake as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_to_lowercase").expect("missing host_str_to_lowercase"),
            host_str_to_lowercase as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_to_uppercase").expect("missing host_str_to_uppercase"),
            host_str_to_uppercase as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_trim").expect("missing host_str_trim"),
            host_str_trim as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_len").expect("missing host_str_len"),
            host_str_len as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_starts_with").expect("missing host_str_starts_with"),
            host_str_starts_with as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_ends_with").expect("missing host_str_ends_with"),
            host_str_ends_with as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_contains").expect("missing host_str_contains"),
            host_str_contains as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_concat").expect("missing host_str_concat"),
            host_str_concat as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_str_replace").expect("missing host_str_replace"),
            host_str_replace as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("channel_send").expect("missing channel_send"),
            host_channel_send as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("channel_recv").expect("missing channel_recv"),
            host_channel_recv as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("spawn").expect("missing spawn"),
            host_spawn as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("result_is_ok").expect("missing result_is_ok"),
            host_result_is_ok as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("result_get_data").expect("missing result_get_data"),
            host_result_get_data as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("result_free").expect("missing result_free"),
            host_result_free as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_map_new").expect("missing host_map_new"),
            host_map_new as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_map_insert").expect("missing host_map_insert"),
            host_map_insert as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_map_get").expect("missing host_map_get"),
            host_map_get as *const () as usize,
        );
        ee.add_global_mapping(
            &module.get_function("host_map_free").expect("missing host_map_free"),
            host_map_free as *const () as usize,
        );

        // Leak the engine into a 'static global for host access (safe because the context lives for the entire process)
        let leaked_ee: ExecutionEngine<'static> = unsafe { std::mem::transmute(ee.clone()) };
        let _ = GLOBAL_ENGINE.set(leaked_ee);

        Ok(ee)
    }
}
