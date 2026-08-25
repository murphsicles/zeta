// src/backend/codegen/jit.rs
//! # JIT & AOT Finalization
//!
//! Final stage of compilation: optimization, execution engine creation, and runtime mapping.
//! Clean, minimal, and production-ready.

use crate::runtime::actor::channel::{
    host_channel_recv, host_channel_send, host_mpsc_channel, host_mpsc_recv, host_mpsc_send,
    host_mpsc_try_recv,
};
use crate::runtime::actor::map::{host_map_get, host_map_insert, host_map_new};
use crate::runtime::actor::result::{host_result_get_data, host_result_is_ok};
use crate::runtime::actor::scheduler::host_spawn;
use crate::runtime::array::{array_free, array_get, array_len, array_new, array_push, array_set};
use crate::runtime::host::{
    host_http_get, host_str_concat, host_str_contains, host_str_ends_with, host_str_len,
    host_str_replace, host_str_starts_with, host_str_to_lowercase, host_str_to_uppercase,
    host_str_trim, host_tls_handshake,
};
// Waker host functions (Linux-only: the reactor module is cfg-gated to Linux).
#[cfg(target_os = "linux")]
use crate::runtime::reactor::{waker_create, waker_wake};
use crate::runtime::std::std_free;
use crate::runtime::zeta_runtime::{
    zeta_array_get_bool, zeta_array_get_i64, zeta_array_set_bool, zeta_array_set_i64,
    zeta_print_i64, zeta_println_i64, zeta_sieve_new,
};
use inkwell::OptimizationLevel;
use inkwell::execution_engine::ExecutionEngine;
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

        // ── Prelude runtime mappings ─────────────────────────────────────
        // First mapping tier: std host implementations for the symbols the
        // codegen may reference. Kept as a single table (single source of
        // truth) so the numbered-duplicate pass below can see every base name.
        let prelude_fns: Vec<(&str, usize)> = {
            let v: Vec<(&str, usize)> = vec![
                ("free", std_free as *const () as usize),
                ("host_str_concat", host_str_concat as *const () as usize),
                (
                    "host_str_to_lowercase",
                    host_str_to_lowercase as *const () as usize,
                ),
                (
                    "host_str_to_uppercase",
                    host_str_to_uppercase as *const () as usize,
                ),
                ("host_str_trim", host_str_trim as *const () as usize),
                ("host_str_len", host_str_len as *const () as usize),
                (
                    "host_str_starts_with",
                    host_str_starts_with as *const () as usize,
                ),
                (
                    "host_str_ends_with",
                    host_str_ends_with as *const () as usize,
                ),
                ("host_str_contains", host_str_contains as *const () as usize),
                ("host_str_replace", host_str_replace as *const () as usize),
                (
                    "host_str_split",
                    crate::runtime::host::host_str_split as *const () as usize,
                ),
                (
                    "host_str_join",
                    crate::runtime::host::host_str_join as *const () as usize,
                ),
                (
                    "host_str_find",
                    crate::runtime::host::host_str_find as *const () as usize,
                ),
                (
                    "host_str_count",
                    crate::runtime::host::host_str_count as *const () as usize,
                ),
                (
                    "host_str_strip",
                    crate::runtime::host::host_str_strip as *const () as usize,
                ),
                (
                    "host_str_lstrip",
                    crate::runtime::host::host_str_lstrip as *const () as usize,
                ),
                (
                    "host_str_rstrip",
                    crate::runtime::host::host_str_rstrip as *const () as usize,
                ),
                (
                    "host_str_isalpha",
                    crate::runtime::host::host_str_isalpha as *const () as usize,
                ),
                (
                    "host_str_isnumeric",
                    crate::runtime::host::host_str_isnumeric as *const () as usize,
                ),
                ("channel_send", host_channel_send as *const () as usize),
                ("channel_recv", host_channel_recv as *const () as usize),
                ("host_mpsc_channel", host_mpsc_channel as *const () as usize),
                ("host_mpsc_send", host_mpsc_send as *const () as usize),
                ("host_mpsc_recv", host_mpsc_recv as *const () as usize),
                (
                    "host_mpsc_try_recv",
                    host_mpsc_try_recv as *const () as usize,
                ),
                ("spawn", host_spawn as *const () as usize),
                ("http_get", host_http_get as *const () as usize),
                ("tls_handshake", host_tls_handshake as *const () as usize),
                ("host_result_is_ok", host_result_is_ok as *const () as usize),
                (
                    "host_result_get_data",
                    host_result_get_data as *const () as usize,
                ),
                ("map_new", host_map_new as *const () as usize),
                ("map_insert", host_map_insert as *const () as usize),
                ("map_get", host_map_get as *const () as usize),
                (
                    "scheduler::init_runtime",
                    crate::runtime::actor::scheduler::init_runtime as *const () as usize,
                ),
                ("array_new", array_new as *const () as usize),
                ("array_push", array_push as *const () as usize),
                ("array_len", array_len as *const () as usize),
                ("array_get", array_get as *const () as usize),
                ("array_set", array_set as *const () as usize),
                ("array_free", array_free as *const () as usize),
                (
                    "future_poll_alloc",
                    crate::runtime::host::future_poll_alloc as *const () as usize,
                ),
                (
                    "future_poll_free",
                    crate::runtime::host::future_poll_free as *const () as usize,
                ),
                (
                    "future_state_get",
                    crate::runtime::host::future_state_get as *const () as usize,
                ),
                (
                    "future_state_set",
                    crate::runtime::host::future_state_set as *const () as usize,
                ),
                (
                    "future_poll",
                    crate::runtime::host::future_poll as *const () as usize,
                ),
                (
                    "future_result",
                    crate::runtime::host::future_result as *const () as usize,
                ),
                (
                    "future_ready",
                    crate::runtime::host::future_ready as *const () as usize,
                ),
                (
                    "zeta_array_get_i64",
                    zeta_array_get_i64 as *const () as usize,
                ),
                (
                    "zeta_array_set_i64",
                    zeta_array_set_i64 as *const () as usize,
                ),
                (
                    "zeta_array_get_bool",
                    zeta_array_get_bool as *const () as usize,
                ),
                (
                    "zeta_array_set_bool",
                    zeta_array_set_bool as *const () as usize,
                ),
                ("zeta_sieve_new", zeta_sieve_new as *const () as usize),
                ("zeta_print_i64", zeta_print_i64 as *const () as usize),
                ("zeta_println_i64", zeta_println_i64 as *const () as usize),
                ("println_i64", zeta_println_i64 as *const () as usize),
            ];
            // Waker host fns exist only on Linux (reactor is Linux-only).
            #[cfg(target_os = "linux")]
            {
                let mut v = v;
                v.push(("create_waker", waker_create as *const () as usize));
                v.push(("wake_waker", waker_wake as *const () as usize));
                v
            }
            #[cfg(not(target_os = "linux"))]
            {
                v
            }
        };
        for (name, fn_ptr) in &prelude_fns {
            if let Some(f) = self.module.get_function(name) {
                ee.add_global_mapping(&f, *fn_ptr);
            }
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

        // ── Explicit runtime mappings ──────────────────────────────────────
        // The codegen declares a fixed set of standard runtime symbols in
        // every module. On Linux some of them resolve via dlsym (the binary
        // is linked with -rdynamic), but macOS does not export process
        // symbols to dlsym — an declared-but-unmapped function resolves to
        // NULL and segfaults when the JITed code calls it. Mapping each
        // symbol explicitly makes JIT resolution platform-independent.
        let explicit: Vec<(&str, usize)> = vec![
            // I/O
            (
                "print_i64",
                crate::runtime::io::print_i64 as *const () as usize,
            ),
            (
                "print_bool",
                crate::runtime::io::print_bool as *const () as usize,
            ),
            (
                "print_str",
                crate::runtime::io::print_str as *const () as usize,
            ),
            ("flush", crate::runtime::io::flush as *const () as usize),
            (
                "test_return_i64",
                crate::runtime::io::test_return_i64 as *const () as usize,
            ),
            // Portable fd/clock helpers
            (
                "set_nonblocking",
                crate::runtime::io::set_nonblocking as *const () as usize,
            ),
            (
                "monotonic_ns",
                crate::runtime::io::monotonic_ns as *const () as usize,
            ),
            (
                "datetime_now",
                crate::runtime::jit_stubs::datetime_now as *const () as usize,
            ),
            (
                "get_time_us",
                crate::runtime::jit_stubs::get_time_us as *const () as usize,
            ),
            // Heap allocator
            (
                "runtime_malloc",
                crate::runtime::host::runtime_malloc as *const () as usize,
            ),
            (
                "runtime_free",
                crate::runtime::memory::runtime_free as *const () as usize,
            ),
            (
                "runtime_calloc",
                crate::runtime::memory::runtime_calloc as *const () as usize,
            ),
            (
                "runtime_realloc",
                crate::runtime::memory::runtime_realloc as *const () as usize,
            ),
            // Result / Option / Map
            (
                "host_result_make_ok",
                crate::runtime::actor::result::host_result_make_ok as *const () as usize,
            ),
            (
                "host_result_make_err",
                crate::runtime::actor::result::host_result_make_err as *const () as usize,
            ),
            (
                "host_result_free",
                crate::runtime::actor::result::host_result_free as *const () as usize,
            ),
            (
                "option_make_some",
                crate::runtime::option::option_make_some as *const () as usize,
            ),
            (
                "option_make_none",
                crate::runtime::option::option_make_none as *const () as usize,
            ),
            (
                "option_is_some",
                crate::runtime::option::option_is_some as *const () as usize,
            ),
            (
                "option_get_data",
                crate::runtime::option::option_get_data as *const () as usize,
            ),
            (
                "option_free",
                crate::runtime::option::option_free as *const () as usize,
            ),
            (
                "map_free",
                crate::runtime::map::map_free as *const () as usize,
            ),
            // Array / stack-array
            (
                "array_set_len",
                crate::runtime::array::array_set_len as *const () as usize,
            ),
            (
                "stack_array_get",
                crate::runtime::array::stack_array_get as *const () as usize,
            ),
            (
                "stack_array_set",
                crate::runtime::array::stack_array_set as *const () as usize,
            ),
            // Host string helpers
            (
                "host_str_count",
                crate::runtime::host::host_str_count as *const () as usize,
            ),
            (
                "host_str_strip",
                crate::runtime::host::host_str_strip as *const () as usize,
            ),
            (
                "host_str_lstrip",
                crate::runtime::host::host_str_lstrip as *const () as usize,
            ),
            (
                "host_str_rstrip",
                crate::runtime::host::host_str_rstrip as *const () as usize,
            ),
            (
                "host_str_isalpha",
                crate::runtime::host::host_str_isalpha as *const () as usize,
            ),
            (
                "host_str_isnumeric",
                crate::runtime::host::host_str_isnumeric as *const () as usize,
            ),
            // Clone / null-check / to-string helpers
            (
                "clone_i64",
                crate::runtime::host::clone_i64 as *const () as usize,
            ),
            (
                "clone_bool",
                crate::runtime::host::clone_bool as *const () as usize,
            ),
            (
                "is_null_i64",
                crate::runtime::host::is_null_i64 as *const () as usize,
            ),
            (
                "is_null_bool",
                crate::runtime::host::is_null_bool as *const () as usize,
            ),
            (
                "to_string_i64",
                crate::runtime::host::to_string_i64 as *const () as usize,
            ),
            (
                "to_string_bool",
                crate::runtime::host::to_string_bool as *const () as usize,
            ),
            (
                "to_string_str",
                crate::runtime::host::to_string_str as *const () as usize,
            ),
            // Stubs (no host implementation exists)
            (
                "time_is_up",
                crate::runtime::jit_stubs::time_is_up as *const () as usize,
            ),
            (
                "print_result",
                crate::runtime::jit_stubs::print_result as *const () as usize,
            ),
            (
                "run_sieve",
                crate::runtime::jit_stubs::run_sieve as *const () as usize,
            ),
            (
                "run_sieve_timed",
                crate::runtime::jit_stubs::run_sieve_timed as *const () as usize,
            ),
            (
                "parallel_sieve",
                crate::runtime::jit_stubs::parallel_sieve as *const () as usize,
            ),
            (
                "parallel_sieve_timed",
                crate::runtime::jit_stubs::parallel_sieve_timed as *const () as usize,
            ),
            (
                "sieve_step",
                crate::runtime::jit_stubs::sieve_step as *const () as usize,
            ),
            (
                "call_i64",
                crate::runtime::jit_stubs::call_i64 as *const () as usize,
            ),
            (
                "avx512_byte_fill",
                crate::runtime::jit_stubs::avx512_byte_fill as *const () as usize,
            ),
            (
                "avx512_count_bits",
                crate::runtime::jit_stubs::avx512_count_bits as *const () as usize,
            ),
            (
                "__builtin_v4i64_andnot",
                crate::runtime::jit_stubs::__builtin_v4i64_andnot as *const () as usize,
            ),
            (
                "__builtin_v4i64_store",
                crate::runtime::jit_stubs::__builtin_v4i64_store as *const () as usize,
            ),
            (
                "read_qword_builtin",
                crate::runtime::jit_stubs::read_qword_builtin as *const () as usize,
            ),
            (
                "write_qword_builtin",
                crate::runtime::jit_stubs::write_qword_builtin as *const () as usize,
            ),
            (
                "clear_bit",
                crate::runtime::jit_stubs::clear_bit as *const () as usize,
            ),
            (
                "test_bit",
                crate::runtime::jit_stubs::test_bit as *const () as usize,
            ),
            // Operator functions (plain + quoted LLVM names)
            (
                "add_i64",
                crate::runtime::jit_stubs::add_i64 as *const () as usize,
            ),
            (
                "+",
                crate::runtime::jit_stubs::add_i64 as *const () as usize,
            ),
            (
                "sub_i64",
                crate::runtime::jit_stubs::sub_i64 as *const () as usize,
            ),
            (
                "-",
                crate::runtime::jit_stubs::sub_i64 as *const () as usize,
            ),
            (
                "mul_i64",
                crate::runtime::jit_stubs::mul_i64 as *const () as usize,
            ),
            (
                "*",
                crate::runtime::jit_stubs::mul_i64 as *const () as usize,
            ),
            (
                "div_i64",
                crate::runtime::jit_stubs::div_i64 as *const () as usize,
            ),
            (
                "/",
                crate::runtime::jit_stubs::div_i64 as *const () as usize,
            ),
            (
                "mod_i64",
                crate::runtime::jit_stubs::mod_i64 as *const () as usize,
            ),
            (
                "%",
                crate::runtime::jit_stubs::mod_i64 as *const () as usize,
            ),
            (
                "and_i64",
                crate::runtime::jit_stubs::and_i64 as *const () as usize,
            ),
            (
                "or_i64",
                crate::runtime::jit_stubs::or_i64 as *const () as usize,
            ),
            (
                "xor_i64",
                crate::runtime::jit_stubs::xor_i64 as *const () as usize,
            ),
            (
                "not_i64",
                crate::runtime::jit_stubs::not_i64 as *const () as usize,
            ),
            (
                "shl_i64",
                crate::runtime::jit_stubs::shl_i64 as *const () as usize,
            ),
            (
                "shr_i64",
                crate::runtime::jit_stubs::shr_i64 as *const () as usize,
            ),
            (
                "eq_i64",
                crate::runtime::jit_stubs::eq_i64 as *const () as usize,
            ),
            (
                "==",
                crate::runtime::jit_stubs::eq_i64 as *const () as usize,
            ),
            (
                "ne_i64",
                crate::runtime::jit_stubs::ne_i64 as *const () as usize,
            ),
            (
                "!=",
                crate::runtime::jit_stubs::ne_i64 as *const () as usize,
            ),
            (
                "lt_i64",
                crate::runtime::jit_stubs::lt_i64 as *const () as usize,
            ),
            ("<", crate::runtime::jit_stubs::lt_i64 as *const () as usize),
            (
                "gt_i64",
                crate::runtime::jit_stubs::gt_i64 as *const () as usize,
            ),
            (">", crate::runtime::jit_stubs::gt_i64 as *const () as usize),
            (
                "le_i64",
                crate::runtime::jit_stubs::le_i64 as *const () as usize,
            ),
            (
                "<=",
                crate::runtime::jit_stubs::le_i64 as *const () as usize,
            ),
            (
                "ge_i64",
                crate::runtime::jit_stubs::ge_i64 as *const () as usize,
            ),
            (
                ">=",
                crate::runtime::jit_stubs::ge_i64 as *const () as usize,
            ),
        ];
        for (name, fn_ptr) in &explicit {
            if let Some(f) = self.module.get_function(name) {
                ee.add_global_mapping(&f, *fn_ptr);
            }
        }

        // Async runtime family: real implementations on Linux, no-op stubs
        // elsewhere (the epoll/timerfd reactor is Linux-only).
        let async_fns: Vec<(&str, usize)> = {
            #[cfg(target_os = "linux")]
            {
                use crate::runtime::reactor as rt;
                vec![
                    ("reactor_create", rt::reactor_create as *const () as usize),
                    ("reactor_add", rt::reactor_add as *const () as usize),
                    ("reactor_remove", rt::reactor_remove as *const () as usize),
                    ("reactor_poll", rt::reactor_poll as *const () as usize),
                    (
                        "reactor_event_fd",
                        rt::reactor_event_fd as *const () as usize,
                    ),
                    (
                        "reactor_event_flags",
                        rt::reactor_event_flags as *const () as usize,
                    ),
                    ("reactor_destroy", rt::reactor_destroy as *const () as usize),
                    ("waker_create", rt::waker_create as *const () as usize),
                    ("waker_wake", rt::waker_wake as *const () as usize),
                    ("waker_consume", rt::waker_consume as *const () as usize),
                    ("waker_destroy", rt::waker_destroy as *const () as usize),
                    (
                        "scheduler_register_waker",
                        rt::scheduler_register_waker as *const () as usize,
                    ),
                    (
                        "scheduler_run_reactor",
                        rt::scheduler_run_reactor as *const () as usize,
                    ),
                ]
            }
            #[cfg(not(target_os = "linux"))]
            {
                use crate::runtime::jit_stubs::async_stubs as st;
                vec![
                    ("reactor_create", st::reactor_create as *const () as usize),
                    ("reactor_add", st::reactor_add as *const () as usize),
                    ("reactor_remove", st::reactor_remove as *const () as usize),
                    ("reactor_poll", st::reactor_poll as *const () as usize),
                    (
                        "reactor_event_fd",
                        st::reactor_event_fd as *const () as usize,
                    ),
                    (
                        "reactor_event_flags",
                        st::reactor_event_flags as *const () as usize,
                    ),
                    ("reactor_destroy", st::reactor_destroy as *const () as usize),
                    ("waker_create", st::waker_create as *const () as usize),
                    ("waker_wake", st::waker_wake as *const () as usize),
                    ("waker_consume", st::waker_consume as *const () as usize),
                    ("waker_destroy", st::waker_destroy as *const () as usize),
                    (
                        "scheduler_register_waker",
                        st::scheduler_register_waker as *const () as usize,
                    ),
                    (
                        "scheduler_run_reactor",
                        st::scheduler_run_reactor as *const () as usize,
                    ),
                ]
            }
        };
        for (name, fn_ptr) in &async_fns {
            if let Some(f) = self.module.get_function(name) {
                ee.add_global_mapping(&f, *fn_ptr);
            }
        }

        // Defensive: LLVM uniquing renames a re-declaration of an existing name to
        // `name.N` (see the std-declaration blocks in codegen.rs). If a call site
        // binds to such a numbered duplicate, it has no mapping above and resolves
        // to NULL (segfault). Map any numbered duplicate of a known runtime symbol
        // to the same target as its base.
        let base_map: std::collections::HashMap<String, usize> = prelude_fns
            .iter()
            .chain(vec_fns.iter())
            .chain(tier2_fns.iter())
            .chain(tier3_fns.iter())
            .chain(explicit.iter())
            .chain(async_fns.iter())
            .map(|(n, p)| (n.to_string(), *p))
            .collect();
        for f in self.module.get_functions() {
            // Declarations only: a numbered runtime duplicate never has a body;
            // user-defined functions must never be remapped.
            if !f.get_basic_blocks().is_empty() {
                continue;
            }
            let name = f.get_name().to_str().unwrap_or("").to_string();
            if let Some(dot) = name.rfind('.') {
                let suffix = &name[dot + 1..];
                if !suffix.is_empty()
                    && suffix.bytes().all(|b| b.is_ascii_digit())
                    && let Some(ptr) = base_map.get(&name[..dot])
                {
                    ee.add_global_mapping(&f, *ptr);
                }
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
    } else if target_str == "x86-64" || target_str == "x86-64-v2" || target_str == "x86-64-v3" {
        // Generic x86-64 target — compatible with any x86-64 CPU
        Target::initialize_native(&InitializationConfig::default())?;
        let triple_str = TargetMachine::get_default_triple();
        (
            triple_str.as_str().to_str().unwrap_or("x86_64").to_string(),
            target_str.to_string(),
            String::new(),
        )
    } else {
        // Native target — optimized for host CPU
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
