// src/backend/codegen/jit.rs
//! # JIT & AOT Finalization
//!
//! Final stage of compilation: optimization, execution engine creation, and runtime mapping.
//! Clean, minimal, and production-ready.

use crate::runtime::actor::channel::{host_channel_recv, host_channel_send};
use crate::runtime::actor::map::{host_map_get, host_map_insert, host_map_new};
use crate::runtime::actor::result::{host_result_get_data, host_result_is_ok};
use crate::runtime::actor::scheduler::host_spawn;
use crate::runtime::host::{
    host_http_get, host_str_concat, host_str_contains, host_str_ends_with, host_str_len,
    host_str_replace, host_str_starts_with, host_str_to_lowercase, host_str_to_uppercase,
    host_str_trim, host_tls_handshake,
};
use crate::runtime::std::std_free;
use inkwell::OptimizationLevel;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::passes::PassManager;
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine};
use std::error::Error;
use std::fs;
use std::path::Path;

impl<'ctx> crate::backend::codegen::LLVMCodegen<'ctx> {
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

        if let Some(f) = self.module.get_function("free") {
            ee.add_global_mapping(&f, std_free as *const () as usize);
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
        if let Some(f) = self.module.get_function("host_str_trim") {
            ee.add_global_mapping(&f, host_str_trim as *const () as usize);
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
        if let Some(f) = self.module.get_function("map_new") {
            ee.add_global_mapping(&f, host_map_new as *const () as usize);
        }
        if let Some(f) = self.module.get_function("map_insert") {
            ee.add_global_mapping(&f, host_map_insert as *const () as usize);
        }
        if let Some(f) = self.module.get_function("map_get") {
            ee.add_global_mapping(&f, host_map_get as *const () as usize);
        }
        if let Some(f) = self.module.get_function("scheduler::init_runtime") {
            ee.add_global_mapping(
                &f,
                crate::runtime::actor::scheduler::init_runtime as *const () as usize,
            );
        }

        Ok(ee)
    }
}

pub fn finalize_and_aot<'ctx>(
    codegen: &crate::backend::codegen::LLVMCodegen<'ctx>,
    path: &Path,
) -> Result<(), Box<dyn Error>> {
    codegen.module.verify()?;

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

    codegen.module.set_triple(&target_triple);
    codegen
        .module
        .set_data_layout(&target_machine.get_target_data().get_data_layout());

    let buffer = target_machine.write_to_memory_buffer(&codegen.module, FileType::Object)?;
    fs::write(path, buffer.as_slice())?;
    Ok(())
}
