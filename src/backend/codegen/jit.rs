// src/backend/codegen/jit.rs
//! # JIT & AOT Finalization
//!
//! Final stage of compilation: optimization, execution engine creation, and runtime mapping.
//! Clean, minimal, and production-ready.

use crate::runtime::actor::channel::{host_channel_recv, host_channel_send, host_mpsc_channel, host_mpsc_recv, host_mpsc_send, host_mpsc_try_recv};
use crate::runtime::actor::map::{host_map_get, host_map_insert, host_map_new};
use crate::runtime::actor::result::{host_result_get_data, host_result_is_ok};
use crate::runtime::actor::scheduler::host_spawn;
use crate::runtime::array::{array_free, array_get, array_len, array_new, array_push, array_set};
use crate::runtime::host::{
    host_http_get, host_str_concat, host_str_contains, host_str_ends_with, host_str_len,
    host_str_replace, host_str_starts_with, host_str_to_lowercase, host_str_to_uppercase,
    host_str_trim, host_tls_handshake,
};
use crate::runtime::std::std_free;
use crate::runtime::zeta_runtime::{
    zeta_array_get_bool, zeta_array_get_i64, zeta_array_set_bool, zeta_array_set_i64,
    zeta_print_i64, zeta_println_i64, zeta_sieve_new,
};
use inkwell::OptimizationLevel;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::passes::PassManager;
use inkwell::targets::{FileType, InitializationConfig, Target, TargetMachine, TargetTriple};
use std::error::Error;
use std::ffi::CString;
use std::fs;
use std::path::Path;

/// Run LLVM's -O3 IR optimization pipeline using the new pass manager.
/// This promotes allocas to SSA (mem2reg), runs instcombine, GVN,
/// loop optimizations, and the full -O3 pipeline.
/// Uses LLVMRunPasses C API directly (LLVM 17+ new PM).
fn optimize_module<'ctx>(module: &inkwell::module::Module<'ctx>, target_machine: &TargetMachine) {
    // Run the full -O3 pipeline on the module via LLVM's new PM pass builder
    unsafe {
        let pipeline = CString::new("default<O3>").unwrap();
        let options = llvm_sys::transforms::pass_builder::LLVMCreatePassBuilderOptions();

        let err = llvm_sys::transforms::pass_builder::LLVMRunPasses(
            module.as_mut_ptr(),
            pipeline.as_ptr(),
            target_machine.as_mut_ptr(),
            options,
        );

        if !err.is_null() {
            // Get error message from LLVMErrorRef
            let msg_ptr = llvm_sys::error::LLVMGetErrorMessage(err);
            let msg = std::ffi::CStr::from_ptr(msg_ptr)
                .to_string_lossy()
                .into_owned();
            llvm_sys::error::LLVMConsumeError(err);
            #[cfg(debug_assertions)]
            eprintln!("[LLVM opt warning: {}]", msg);
        }
    }
}

impl<'ctx> crate::backend::codegen::LLVMCodegen<'ctx> {
    pub fn finalize_and_jit(
        &mut self,
        target_str: &str,
    ) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        if target_str == "wasm32" {
            return Err("JIT not supported for WASM target. Use AOT compilation instead.".into());
        }
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

        // Run the full LLVM optimization pipeline before JIT
        optimize_module(&self.module, &target_machine);

        // Debug: print module IR
        // self.module.print_to_stderr();

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
        if let Some(f) = self.module.get_function("host_str_split") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_split as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("host_str_join") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_join as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("host_str_find") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_find as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("host_str_count") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_count as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("host_str_strip") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_strip as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("host_str_lstrip") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_lstrip as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("host_str_rstrip") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_rstrip as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("host_str_isalpha") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_isalpha as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("host_str_isnumeric") {
            ee.add_global_mapping(
                &f,
                crate::runtime::host::host_str_isnumeric as *const () as usize,
            );
        }
        if let Some(f) = self.module.get_function("channel_send") {
            ee.add_global_mapping(&f, host_channel_send as *const () as usize);
        }
        if let Some(f) = self.module.get_function("channel_recv") {
            ee.add_global_mapping(&f, host_channel_recv as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_mpsc_channel") {
            ee.add_global_mapping(&f, host_mpsc_channel as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_mpsc_send") {
            ee.add_global_mapping(&f, host_mpsc_send as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_mpsc_recv") {
            ee.add_global_mapping(&f, host_mpsc_recv as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_mpsc_try_recv") {
            ee.add_global_mapping(&f, host_mpsc_try_recv as *const () as usize);
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
        if let Some(f) = self.module.get_function("host_result_is_ok") {
            ee.add_global_mapping(&f, host_result_is_ok as *const () as usize);
        }
        if let Some(f) = self.module.get_function("host_result_get_data") {
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

        // Array runtime functions
        if let Some(f) = self.module.get_function("array_new") {
            ee.add_global_mapping(&f, array_new as *const () as usize);
        }
        if let Some(f) = self.module.get_function("array_push") {
            ee.add_global_mapping(&f, array_push as *const () as usize);
        }
        if let Some(f) = self.module.get_function("array_len") {
            ee.add_global_mapping(&f, array_len as *const () as usize);
        }
        if let Some(f) = self.module.get_function("array_get") {
            ee.add_global_mapping(&f, array_get as *const () as usize);
        }
        if let Some(f) = self.module.get_function("array_set") {
            ee.add_global_mapping(&f, array_set as *const () as usize);
        }
        if let Some(f) = self.module.get_function("array_free") {
            ee.add_global_mapping(&f, array_free as *const () as usize);
        }

        // Async/future runtime functions
        if let Some(f) = self.module.get_function("future_poll_alloc") {
            ee.add_global_mapping(&f, crate::runtime::host::future_poll_alloc as *const () as usize);
        }
        if let Some(f) = self.module.get_function("future_poll_free") {
            ee.add_global_mapping(&f, crate::runtime::host::future_poll_free as *const () as usize);
        }
        if let Some(f) = self.module.get_function("future_state_get") {
            ee.add_global_mapping(&f, crate::runtime::host::future_state_get as *const () as usize);
        }
        if let Some(f) = self.module.get_function("future_state_set") {
            ee.add_global_mapping(&f, crate::runtime::host::future_state_set as *const () as usize);
        }
        if let Some(f) = self.module.get_function("future_poll") {
            ee.add_global_mapping(&f, crate::runtime::host::future_poll as *const () as usize);
        }
        if let Some(f) = self.module.get_function("future_result") {
            ee.add_global_mapping(&f, crate::runtime::host::future_result as *const () as usize);
        }
        if let Some(f) = self.module.get_function("future_ready") {
            ee.add_global_mapping(&f, crate::runtime::host::future_ready as *const () as usize);
        }

        // Zeta runtime mapping
        if let Some(f) = self.module.get_function("zeta_array_get_i64") {
            ee.add_global_mapping(&f, zeta_array_get_i64 as *const () as usize);
        }
        if let Some(f) = self.module.get_function("zeta_array_set_i64") {
            ee.add_global_mapping(&f, zeta_array_set_i64 as *const () as usize);
        }
        if let Some(f) = self.module.get_function("zeta_array_get_bool") {
            ee.add_global_mapping(&f, zeta_array_get_bool as *const () as usize);
        }
        if let Some(f) = self.module.get_function("zeta_array_set_bool") {
            ee.add_global_mapping(&f, zeta_array_set_bool as *const () as usize);
        }
        if let Some(f) = self.module.get_function("zeta_sieve_new") {
            ee.add_global_mapping(&f, zeta_sieve_new as *const () as usize);
        }
        if let Some(f) = self.module.get_function("zeta_print_i64") {
            ee.add_global_mapping(&f, zeta_print_i64 as *const () as usize);
        }
        if let Some(f) = self.module.get_function("zeta_println_i64") {
            ee.add_global_mapping(&f, zeta_println_i64 as *const () as usize);
        }
        if let Some(f) = self.module.get_function("println_i64") {
            ee.add_global_mapping(&f, zeta_println_i64 as *const () as usize);
        }

        // Map Vec runtime functions (zeta_vec_* → vec_*)
        let vec_fns: Vec<(&str, usize)> = vec![
            (
                "vec_new",
                crate::runtime::vec::zeta_vec_new as *const () as usize,
            ),
            (
                "vec_push",
                crate::runtime::vec::zeta_vec_push as *const () as usize,
            ),
            (
                "vec_pop",
                crate::runtime::vec::zeta_vec_pop as *const () as usize,
            ),
            (
                "vec_get",
                crate::runtime::vec::zeta_vec_get as *const () as usize,
            ),
            (
                "vec_set",
                crate::runtime::vec::zeta_vec_set as *const () as usize,
            ),
            (
                "vec_len",
                crate::runtime::vec::zeta_vec_len as *const () as usize,
            ),
            (
                "vec_capacity",
                crate::runtime::vec::zeta_vec_capacity as *const () as usize,
            ),
            (
                "vec_clear",
                crate::runtime::vec::zeta_vec_clear as *const () as usize,
            ),
            (
                "vec_free",
                crate::runtime::vec::zeta_vec_free as *const () as usize,
            ),
        ];
        for (name, fn_ptr) in &vec_fns {
            if let Some(f) = self.module.get_function(name) {
                ee.add_global_mapping(&f, *fn_ptr);
            }
        }
        // Map monomorphized vec_* functions
        for func_name in self.module.get_functions() {
            let name = func_name.get_name().to_str().unwrap().to_string();
            if name.starts_with("vec_push_") {
                ee.add_global_mapping(
                    &func_name,
                    crate::runtime::vec::zeta_vec_push as *const () as usize,
                );
            }
            if name.starts_with("vec_get_") {
                ee.add_global_mapping(
                    &func_name,
                    crate::runtime::vec::zeta_vec_get as *const () as usize,
                );
            }
            if name.starts_with("vec_len_") {
                ee.add_global_mapping(
                    &func_name,
                    crate::runtime::vec::zeta_vec_len as *const () as usize,
                );
            }
        }

        // Map Tier 2 runtime functions (fs, path, net, atomic)
        let tier2_fns: Vec<(&str, usize)> = vec![
            (
                "fs_read_to_string",
                crate::runtime::fs::fs_read_to_string as *const () as usize,
            ),
            (
                "fs_write",
                crate::runtime::fs::fs_write as *const () as usize,
            ),
            (
                "fs_create_dir",
                crate::runtime::fs::fs_create_dir as *const () as usize,
            ),
            (
                "fs_create_dir_all",
                crate::runtime::fs::fs_create_dir_all as *const () as usize,
            ),
            (
                "fs_remove_file",
                crate::runtime::fs::fs_remove_file as *const () as usize,
            ),
            (
                "fs_remove_dir",
                crate::runtime::fs::fs_remove_dir as *const () as usize,
            ),
            (
                "fs_rename",
                crate::runtime::fs::fs_rename as *const () as usize,
            ),
            ("fs_copy", crate::runtime::fs::fs_copy as *const () as usize),
            (
                "fs_exists",
                crate::runtime::fs::fs_exists as *const () as usize,
            ),
            (
                "fs_is_file",
                crate::runtime::fs::fs_is_file as *const () as usize,
            ),
            (
                "fs_is_dir",
                crate::runtime::fs::fs_is_dir as *const () as usize,
            ),
            (
                "fs_metadata_len",
                crate::runtime::fs::fs_metadata_len as *const () as usize,
            ),
            (
                "fs_read_dir",
                crate::runtime::fs::fs_read_dir as *const () as usize,
            ),
            (
                "fs_canonicalize",
                crate::runtime::fs::fs_canonicalize as *const () as usize,
            ),
            (
                "path_parent",
                crate::runtime::path::path_parent as *const () as usize,
            ),
            (
                "path_file_name",
                crate::runtime::path::path_file_name as *const () as usize,
            ),
            (
                "path_extension",
                crate::runtime::path::path_extension as *const () as usize,
            ),
            (
                "path_join",
                crate::runtime::path::path_join as *const () as usize,
            ),
            (
                "path_is_absolute",
                crate::runtime::path::path_is_absolute as *const () as usize,
            ),
            (
                "path_as_str",
                crate::runtime::path::path_as_str as *const () as usize,
            ),
            (
                "tcp_connect",
                crate::runtime::net::tcp_connect as *const () as usize,
            ),
            (
                "tcp_write",
                crate::runtime::net::tcp_write as *const () as usize,
            ),
            (
                "tcp_read",
                crate::runtime::net::tcp_read as *const () as usize,
            ),
            (
                "tcp_close",
                crate::runtime::net::tcp_close as *const () as usize,
            ),
            (
                "tcp_bind",
                crate::runtime::net::tcp_bind as *const () as usize,
            ),
            (
                "tcp_accept",
                crate::runtime::net::tcp_accept as *const () as usize,
            ),
            (
                "host_str_all_interfaces",
                crate::runtime::host::host_str_all_interfaces as *const () as usize,
            ),
            (
                "atomic_bool_new",
                crate::runtime::atomic::atomic_bool_new as *const () as usize,
            ),
            (
                "atomic_bool_load",
                crate::runtime::atomic::atomic_bool_load as *const () as usize,
            ),
            (
                "atomic_bool_store",
                crate::runtime::atomic::atomic_bool_store as *const () as usize,
            ),
            (
                "atomic_i64_new",
                crate::runtime::atomic::atomic_i64_new as *const () as usize,
            ),
            (
                "atomic_i64_load",
                crate::runtime::atomic::atomic_i64_load as *const () as usize,
            ),
            (
                "atomic_i64_store",
                crate::runtime::atomic::atomic_i64_store as *const () as usize,
            ),
            (
                "atomic_i64_swap",
                crate::runtime::atomic::atomic_i64_swap as *const () as usize,
            ),
            (
                "atomic_i64_cas",
                crate::runtime::atomic::atomic_i64_cas as *const () as usize,
            ),
            (
                "atomic_i64_add",
                crate::runtime::atomic::atomic_i64_fetch_add as *const () as usize,
            ),
            (
                "atomic_i64_sub",
                crate::runtime::atomic::atomic_i64_fetch_sub as *const () as usize,
            ),
        ];
        for (name, fn_ptr) in &tier2_fns {
            if let Some(f) = self.module.get_function(name) {
                ee.add_global_mapping(&f, *fn_ptr);
            }
        }

        // Map Tier 3 runtime functions (char, duration, process, thread)
        let tier3_fns: Vec<(&str, usize)> = vec![
            (
                "char_is_digit",
                crate::runtime::char_::char_is_digit as *const () as usize,
            ),
            (
                "char_is_alphabetic",
                crate::runtime::char_::char_is_alphabetic as *const () as usize,
            ),
            (
                "char_is_alphanumeric",
                crate::runtime::char_::char_is_alphanumeric as *const () as usize,
            ),
            (
                "char_is_lowercase",
                crate::runtime::char_::char_is_lowercase as *const () as usize,
            ),
            (
                "char_is_uppercase",
                crate::runtime::char_::char_is_uppercase as *const () as usize,
            ),
            (
                "char_is_whitespace",
                crate::runtime::char_::char_is_whitespace as *const () as usize,
            ),
            (
                "char_to_lowercase",
                crate::runtime::char_::char_to_lowercase as *const () as usize,
            ),
            (
                "char_to_uppercase",
                crate::runtime::char_::char_to_uppercase as *const () as usize,
            ),
            (
                "char_from_u32",
                crate::runtime::char_::char_from_u32 as *const () as usize,
            ),
            (
                "char_to_digit",
                crate::runtime::char_::char_to_digit as *const () as usize,
            ),
            (
                "char_is_control",
                crate::runtime::char_::char_is_control as *const () as usize,
            ),
            (
                "char_is_numeric",
                crate::runtime::char_::char_is_numeric as *const () as usize,
            ),
            (
                "duration_add",
                crate::runtime::duration::duration_add as *const () as usize,
            ),
            (
                "duration_sub",
                crate::runtime::duration::duration_sub as *const () as usize,
            ),
            (
                "duration_mul",
                crate::runtime::duration::duration_mul as *const () as usize,
            ),
            (
                "duration_div",
                crate::runtime::duration::duration_div as *const () as usize,
            ),
            (
                "duration_lt",
                crate::runtime::duration::duration_lt as *const () as usize,
            ),
            (
                "duration_eq",
                crate::runtime::duration::duration_eq as *const () as usize,
            ),
            (
                "process_command_new",
                crate::runtime::process::process_command_new as *const () as usize,
            ),
            (
                "process_command_arg",
                crate::runtime::process::process_command_arg as *const () as usize,
            ),
            (
                "process_command_output",
                crate::runtime::process::process_command_output as *const () as usize,
            ),
            (
                "process_command_status",
                crate::runtime::process::process_command_status as *const () as usize,
            ),
            (
                "process_output_stdout",
                crate::runtime::process::process_output_stdout as *const () as usize,
            ),
            (
                "process_output_stderr",
                crate::runtime::process::process_output_stderr as *const () as usize,
            ),
            (
                "process_output_status",
                crate::runtime::process::process_output_status as *const () as usize,
            ),
            (
                "thread_spawn",
                crate::runtime::thread_::thread_spawn as *const () as usize,
            ),
            (
                "thread_join",
                crate::runtime::thread_::thread_join as *const () as usize,
            ),
            (
                "thread_sleep_ms",
                crate::runtime::thread_::thread_sleep_ms as *const () as usize,
            ),
        ];
        for (name, fn_ptr) in &tier3_fns {
            if let Some(f) = self.module.get_function(name) {
                ee.add_global_mapping(&f, *fn_ptr);
            }
        }

        Ok(ee)
    }
}

pub fn finalize_and_aot<'ctx>(
    codegen: &crate::backend::codegen::LLVMCodegen<'ctx>,
    path: &Path,
    target_str: &str,
) -> Result<(), Box<dyn Error>> {
    codegen.module.verify()?;

    let (triple, cpu, features) = if target_str == "wasm32" || target_str == "wasm32-wasi" {
        // WASM targets — initialize all targets to register WebAssembly
        Target::initialize_all(&InitializationConfig::default());
        let triple = if target_str == "wasm32-wasi" {
            "wasm32-wasi"
        } else {
            "wasm32-unknown-unknown"
        };
        (
            triple.to_string(),
            "generic".to_string(),
            "+bulk-memory,+simd128".to_string(),
        )
    } else {
        // Native target
        Target::initialize_native(&InitializationConfig::default())?;
        // Use default triple directly (as TargetTriple, not String)
        let triple_str = TargetMachine::get_default_triple();
        (
            triple_str.as_str().to_str().unwrap_or("x86_64").to_string(),
            TargetMachine::get_host_cpu_name().to_string(),
            TargetMachine::get_host_cpu_features().to_string(),
        )
    };

    let target_triple = TargetTriple::create(&triple);
    let target = Target::from_triple(&target_triple)?;
    let target_machine = target
        .create_target_machine(
            &target_triple,
            &cpu,
            &features,
            OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .ok_or("Failed to create target machine")?;

    codegen.module.set_triple(&target_triple);
    codegen
        .module
        .set_data_layout(&target_machine.get_target_data().get_data_layout());

    // Run the full LLVM optimization pipeline before codegen
    optimize_module(&codegen.module, &target_machine);

    let buffer = target_machine.write_to_memory_buffer(&codegen.module, FileType::Object)?;
    fs::write(path, buffer.as_slice())?;
    Ok(())
}
