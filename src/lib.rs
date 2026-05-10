// src/lib.rs
//! # Zeta Compiler Library
//!
//! The core library for the Zeta systems programming language.
//! This crate provides a complete pipeline:

#![allow(dead_code)]
#![allow(unused_must_use)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]
#![allow(unused_unsafe)]
#![allow(clippy::too_many_arguments)]
#![allow(clippy::type_complexity)]
#![allow(clippy::new_without_default)]
#![allow(clippy::if_same_then_else)]
#![allow(clippy::manual_strip)]
#![allow(clippy::large_enum_variant)]
#![allow(clippy::needless_pass_by_value)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::unnecessary_cast)]
#![allow(clippy::vec_init_then_push)]
#![allow(clippy::assertions_on_constants)]
#![allow(clippy::len_without_is_empty)]
#![allow(clippy::should_implement_trait)]
//!   1. Parsing â†’ AST
//!   2. Resolution + monomorphization + MIR lowering
//!   3. LLVM code generation + JIT/AOT
//!
//! This is the **foundational bedrock** for all future Zeta self-hosting.

#![allow(clippy::missing_safety_doc)]
#![allow(unused_variables)]
#![allow(clippy::cloned_ref_to_slice_refs)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::collapsible_match)]
#![allow(clippy::doc_lazy_continuation)]
#![allow(clippy::empty_line_after_doc_comments)]
#![allow(clippy::explicit_counter_loop)]
#![allow(clippy::manual_clamp)]
#![allow(clippy::match_like_matches_macro)]
#![allow(clippy::needless_range_loop)]
#![allow(clippy::new_ret_no_self)]
#![allow(clippy::nonminimal_bool)]
#![allow(clippy::only_used_in_recursion)]
#![allow(clippy::result_large_err)]
#![allow(clippy::unnecessary_get_then_check)]
#![allow(clippy::unnecessary_sort_by)]
//! Every line is optimized for speed, simplicity, and clarity.

pub mod backend;
#[cfg(feature = "blockchain")]
pub mod blockchain; // Enabled for Teranode integration
pub mod compiler_config;
pub mod debugger;
pub mod diagnostics;
pub mod error_codes;
pub mod frontend;
pub mod integration;
pub mod lsp;
pub mod middle;
pub mod ml;
pub mod package;
// pub mod memory;  // Temporarily disabled due to compilation errors
pub mod distributed;
pub mod runtime;
pub mod std;
pub mod workflows;
pub mod zeta;

// Paradigm-shifting features (simplified implementation)
pub mod paradigm_simple;

// #[cfg(test)]
// mod tests;

pub use backend::codegen::LLVMCodegen;
pub use frontend::ast::AstNode;
pub use frontend::parser::top_level::parse_zeta;
pub use middle::mir::mir::Mir;
pub use middle::resolver::resolver::Resolver;
pub use runtime::actor::result;
pub use runtime::actor::scheduler::{init_runtime, spawn};
pub use runtime::option;

use crate::middle::ctfe::error::CtfeError;
use inkwell::context::Context;

/// Compiles a Zeta source string to executable code and runs `main()`.
///
/// This is the primary public API used by the bootstrap `main.rs` and will
/// become the entry point when Zeta becomes fully self-hosted.
pub fn compile_and_run_zeta(code: &str) -> Result<i64, String> {
    init_runtime();
    // crate::runtime::r#async::init_async_runtime(); // TODO: Enable when async is fully implemented

    let (remaining, asts) = parse_zeta(code).map_err(|e| format!("Parse error: {:?}", e))?;

    // Check that all input was consumed (allow only whitespace/comments)
    let trimmed_remaining = remaining.trim();
    if !trimmed_remaining.is_empty() {
        return Err(format!(
            "Syntax error: incomplete parse. Remaining: '{}'",
            trimmed_remaining
        ));
    }

    let mut resolver = Resolver::new();

    // Expand macros before registration and type checking
    let expanded_asts = resolver
        .expand_macros(&asts)
        .map_err(|e| format!("Macro expansion error: {}", e))?;

    // Evaluate constants at compile time
    let const_evaluated_asts = crate::middle::const_eval::evaluate_constants(&expanded_asts)
        .map_err(|e: CtfeError| format!("Const evaluation error: {}", e))?;

    for ast in &const_evaluated_asts {
        resolver.register(ast.clone());
    }
    if !resolver.typecheck(&const_evaluated_asts) {
        return Err("Typecheck failed".into());
    }

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta");

    // Generate MIR for all registered functions
    let mut mirs = Vec::new();

    // Get all registered function ASTs (including module functions)
    let registered_funcs = resolver.get_registered_funcs();

    for ast in registered_funcs {
        if let AstNode::FuncDef { name, .. } = &ast {
            let mir = resolver.lower_to_mir(&ast);
            mirs.push(mir);
        }
    }

    // Check if we have a main function (for backward compatibility with tests)
    let has_main = const_evaluated_asts
        .iter()
        .any(|a| matches!(a, AstNode::FuncDef { name, .. } if name == "main"));
    if !has_main {
        return Err("No main function".to_string());
    }

    codegen.gen_mirs(&mirs);

    let ee = codegen
        .finalize_and_jit("native")
        .map_err(|e| e.to_string())?;

    // Map required runtime functions
    if let Some(f) = codegen.module.get_function("malloc") {
        let malloc_fn = crate::runtime::std::std_malloc as *const () as usize;
        ee.add_global_mapping(&f, malloc_fn);
    }
    // Map monomorphized malloc functions (e.g., malloc_u64, malloc_bool, etc.)
    for func_name in codegen.module.get_functions() {
        let name = func_name.get_name().to_str().unwrap();
        if name.starts_with("malloc_") {
            let malloc_fn = crate::runtime::std::std_malloc as *const () as usize;
            ee.add_global_mapping(&func_name, malloc_fn);
        }
    }
    if let Some(f) = codegen.module.get_function("free") {
        let free_fn = crate::runtime::std::std_free as *const () as usize;
        ee.add_global_mapping(&f, free_fn);
    }
    // Map monomorphized free functions
    for func_name in codegen.module.get_functions() {
        let name = func_name.get_name().to_str().unwrap();
        if name.starts_with("free_") {
            let free_fn = crate::runtime::std::std_free as *const () as usize;
            ee.add_global_mapping(&func_name, free_fn);
        }
    }
    if let Some(f) = codegen.module.get_function("print") {
        let print_fn = crate::runtime::std::std_print as *const () as usize;
        ee.add_global_mapping(&f, print_fn);
    }
    if let Some(f) = codegen.module.get_function("println") {
        let println_fn = crate::runtime::std::std_println as *const () as usize;
        ee.add_global_mapping(&f, println_fn);
    }
    if let Some(f) = codegen.module.get_function("println_i64") {
        let println_i64_fn = crate::runtime::io::println_i64 as *const () as usize;
        ee.add_global_mapping(&f, println_i64_fn);
    }
    if let Some(f) = codegen.module.get_function("args") {
        let args_fn = crate::runtime::std::std_args as *const () as usize;
        ee.add_global_mapping(&f, args_fn);
    }
    // Map map_get function for const arrays
    if let Some(f) = codegen.module.get_function("map_get") {
        let map_get_fn = crate::runtime::map::map_get as *const () as usize;
        ee.add_global_mapping(&f, map_get_fn);
    }
    // Map Option runtime functions
    if let Some(f) = codegen.module.get_function("option_make_some") {
        let fn_ptr = crate::runtime::option::option_make_some as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("option_make_none") {
        let fn_ptr = crate::runtime::option::option_make_none as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("option_is_some") {
        let fn_ptr = crate::runtime::option::option_is_some as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("option_get_data") {
        let fn_ptr = crate::runtime::option::option_get_data as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("option_free") {
        let fn_ptr = crate::runtime::option::option_free as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    // Map Result runtime functions
    if let Some(f) = codegen.module.get_function("host_result_make_ok") {
        let fn_ptr = crate::runtime::actor::result::host_result_make_ok as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("host_result_make_err") {
        let fn_ptr = crate::runtime::actor::result::host_result_make_err as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("host_result_is_ok") {
        let fn_ptr = crate::runtime::actor::result::host_result_is_ok as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("host_result_get_data") {
        let fn_ptr = crate::runtime::actor::result::host_result_get_data as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("host_result_free") {
        let fn_ptr = crate::runtime::actor::result::host_result_free as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }

    // Map bit operation intrinsics
    if let Some(f) = codegen.module.get_function("intrinsic_tzcnt_u64") {
        let fn_ptr = crate::runtime::std::intrinsic_tzcnt_u64 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_lzcnt_u64") {
        let fn_ptr = crate::runtime::std::intrinsic_lzcnt_u64 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_popcnt_u64") {
        let fn_ptr = crate::runtime::std::intrinsic_popcnt_u64 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_bsf_u64") {
        let fn_ptr = crate::runtime::std::intrinsic_bsf_u64 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_bsr_u64") {
        let fn_ptr = crate::runtime::std::intrinsic_bsr_u64 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_rotl_u64") {
        let fn_ptr = crate::runtime::std::intrinsic_rotl_u64 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_rotr_u64") {
        let fn_ptr = crate::runtime::std::intrinsic_rotr_u64 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_bswap_u64") {
        let fn_ptr = crate::runtime::std::intrinsic_bswap_u64 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_clflush") {
        let fn_ptr = crate::runtime::std::intrinsic_clflush as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_mfence") {
        let fn_ptr = crate::runtime::std::intrinsic_mfence as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_lfence") {
        let fn_ptr = crate::runtime::std::intrinsic_lfence as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_sfence") {
        let fn_ptr = crate::runtime::std::intrinsic_sfence as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("intrinsic_pause") {
        let fn_ptr = crate::runtime::std::intrinsic_pause as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }

    // Map SIMD runtime functions
    if let Some(f) = codegen.module.get_function("simd_splat_i32x4") {
        let fn_ptr = crate::runtime::std::simd_splat_i32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_splat_i64x2") {
        let fn_ptr = crate::runtime::std::simd_splat_i64x2 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_splat_f32x4") {
        let fn_ptr = crate::runtime::std::simd_splat_f32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_add_i32x4") {
        let fn_ptr = crate::runtime::std::simd_add_i32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_mul_i32x4") {
        let fn_ptr = crate::runtime::std::simd_mul_i32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_sub_i32x4") {
        let fn_ptr = crate::runtime::std::simd_sub_i32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_load_i32x4") {
        let fn_ptr = crate::runtime::std::simd_load_i32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_store_i32x4") {
        let fn_ptr = crate::runtime::std::simd_store_i32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_extract_i32x4") {
        let fn_ptr = crate::runtime::std::simd_extract_i32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }
    if let Some(f) = codegen.module.get_function("simd_insert_i32x4") {
        let fn_ptr = crate::runtime::std::simd_insert_i32x4 as *const () as usize;
        ee.add_global_mapping(&f, fn_ptr);
    }

    // Map atomic operations (commented out until implemented)
    // if let Some(f) = codegen.module.get_function("host_atomic_bool_new") {
    //     let fn_ptr = crate::runtime::sync::host_atomic_bool_new as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_atomic_bool_load") {
    //     let fn_ptr = crate::runtime::sync::host_atomic_bool_load as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_atomic_bool_store") {
    //     let fn_ptr = crate::runtime::sync::host_atomic_bool_store as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_atomic_usize_new") {
    //     let fn_ptr = crate::runtime::sync::host_atomic_usize_new as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_atomic_usize_load") {
    //     let fn_ptr = crate::runtime::sync::host_atomic_usize_load as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_atomic_usize_store") {
    //     let fn_ptr = crate::runtime::sync::host_atomic_usize_store as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_atomic_usize_fetch_add") {
    //     let fn_ptr = crate::runtime::sync::host_atomic_usize_fetch_add as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_atomic_usize_fetch_sub") {
    //     let fn_ptr = crate::runtime::sync::host_atomic_usize_fetch_sub as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }

    // Map mpsc channel functions (commented out until implemented)
    // if let Some(f) = codegen.module.get_function("host_mpsc_channel") {
    //     let fn_ptr = crate::runtime::mpsc::host_mpsc_channel as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_mpsc_send") {
    //     let fn_ptr = crate::runtime::mpsc::host_mpsc_send as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_mpsc_recv") {
    //     let fn_ptr = crate::runtime::mpsc::host_mpsc_recv as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_mpsc_try_recv") {
    //     let fn_ptr = crate::runtime::mpsc::host_mpsc_try_recv as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }
    // if let Some(f) = codegen.module.get_function("host_mpsc_clone_sender") {
    //     let fn_ptr = crate::runtime::mpsc::host_mpsc_clone_sender as *const () as usize;
    //     ee.add_global_mapping(&f, fn_ptr);
    // }

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
        if let Some(f) = codegen.module.get_function(name) {
            ee.add_global_mapping(&f, *fn_ptr);
        }
    }
    // Map monomorphized vec_* functions
    for func_name in codegen.module.get_functions() {
        let name = func_name.get_name().to_str().unwrap();
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
        // Filesystem
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
        // Path
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
            "path_has_extension",
            crate::runtime::path::path_has_extension as *const () as usize,
        ),
        (
            "path_is_absolute",
            crate::runtime::path::path_is_absolute as *const () as usize,
        ),
        (
            "path_join",
            crate::runtime::path::path_join as *const () as usize,
        ),
        (
            "path_absolute",
            crate::runtime::path::path_absolute as *const () as usize,
        ),
        (
            "path_as_str",
            crate::runtime::path::path_as_str as *const () as usize,
        ),
        (
            "path_set_extension",
            crate::runtime::path::path_set_extension as *const () as usize,
        ),
        // Networking
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
            "tcp_peer_addr",
            crate::runtime::net::tcp_peer_addr as *const () as usize,
        ),
        (
            "tcp_local_addr",
            crate::runtime::net::tcp_local_addr as *const () as usize,
        ),
        // Atomics
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
            "atomic_bool_swap",
            crate::runtime::atomic::atomic_bool_swap as *const () as usize,
        ),
        (
            "atomic_bool_cas",
            crate::runtime::atomic::atomic_bool_cas as *const () as usize,
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
        (
            "atomic_i64_and",
            crate::runtime::atomic::atomic_i64_fetch_and as *const () as usize,
        ),
        (
            "atomic_i64_or",
            crate::runtime::atomic::atomic_i64_fetch_or as *const () as usize,
        ),
    ];
    for (name, fn_ptr) in &tier2_fns {
        if let Some(f) = codegen.module.get_function(name) {
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
        if let Some(f) = codegen.module.get_function(name) {
            ee.add_global_mapping(&f, *fn_ptr);
        }
    }

    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee
            .get_function::<MainFn>("main")
            .map_err(|_| "No main".to_string())?;
        Ok(main.call())
    }
}

// Diagnostic macros — emit to thread-local DiagnosticReporter
#[macro_export]
macro_rules! diag_error {
    ($code:expr, $($arg:tt)*) => {{
        use $crate::diagnostics::{Diagnostic, Severity};
        let diag = Diagnostic {
            severity: Severity::Error,
            code: Some(format!("E{}", $code)),
            message: format!($($arg)*),
            span: None,
            context: None,
            help: None,
            note: None,
            suggestions: vec![],
        };
        $crate::diagnostics::emit(diag);
    }};
}

#[macro_export]
macro_rules! diag_warning {
    ($code:expr, $($arg:tt)*) => {{
        use $crate::diagnostics::{Diagnostic, Severity};
        let diag = Diagnostic {
            severity: Severity::Warning,
            code: Some(format!("W{}", $code)),
            message: format!($($arg)*),
            span: None,
            context: None,
            help: None,
            note: None,
            suggestions: vec![],
        };
        $crate::diagnostics::emit(diag);
    }};
}

/// Compiles Zeta source with enhanced error reporting
///
/// Returns either the execution result or formatted diagnostics
pub fn compile_with_diagnostics(code: &str, filename: &'static str) -> Result<i64, String> {
    use crate::diagnostics::{DiagnosticReporter, SourceLocation, SourceSpan};
    use crate::error_codes::diagnostic_from_code;

    let mut reporter = DiagnosticReporter::new();
    reporter.add_source(filename, code.to_string());

    init_runtime();

    // Parse with location tracking
    let (remaining, asts) = parse_zeta(code).map_err(|e| {
        // Create a diagnostic for parse error
        let span = SourceSpan::single(SourceLocation::new(filename, 1, 1, 0));
        let diag = diagnostic_from_code("E1001", format!("Parse error: {:?}", e), Some(span));
        reporter.report(diag);
        reporter.format_all()
    })?;

    // Check for incomplete parse
    let trimmed_remaining = remaining.trim();
    if !trimmed_remaining.is_empty() {
        // Estimate location (simplified - would need actual parser tracking)
        let lines_parsed = code.lines().count() - remaining.lines().count();
        let span = SourceSpan::single(SourceLocation::new(
            filename,
            lines_parsed + 1,
            1,
            code.len() - remaining.len(),
        ));

        let diag = diagnostic_from_code(
            "E1001",
            format!(
                "Syntax error: incomplete parse. Remaining: '{}'",
                trimmed_remaining
            ),
            Some(span),
        );
        reporter.report(diag);
    }

    if reporter.has_errors() {
        return Err(reporter.format_all());
    }

    let mut resolver = Resolver::new();

    // Expand macros
    let expanded_asts = resolver.expand_macros(&asts).map_err(|e| {
        let span = SourceSpan::single(SourceLocation::new(filename, 1, 1, 0));
        let diag =
            diagnostic_from_code("E3001", format!("Macro expansion error: {}", e), Some(span));
        reporter.report(diag);
        reporter.format_all()
    })?;

    // Evaluate constants
    let const_evaluated_asts = crate::middle::const_eval::evaluate_constants(&expanded_asts)
        .map_err(|e: CtfeError| {
            let span = SourceSpan::single(SourceLocation::new(filename, 1, 1, 0));
            let diag = diagnostic_from_code(
                "E2003",
                format!("Const evaluation error: {}", e),
                Some(span),
            );
            reporter.report(diag);
            reporter.format_all()
        })?;

    // Register and typecheck
    for ast in &const_evaluated_asts {
        resolver.register(ast.clone());
    }

    if !resolver.typecheck(&const_evaluated_asts) {
        // TODO: Collect type errors from resolver
        let span = SourceSpan::single(SourceLocation::new(filename, 1, 1, 0));
        let diag = diagnostic_from_code("E2001", "Typecheck failed".to_string(), Some(span));
        reporter.report(diag);
        return Err(reporter.format_all());
    }

    // Check for main function
    let has_main = const_evaluated_asts
        .iter()
        .any(|a| matches!(a, AstNode::FuncDef { name, .. } if name == "main"));
    if !has_main {
        let span = SourceSpan::single(SourceLocation::new(filename, 1, 1, 0));
        let diag = diagnostic_from_code("E4001", "No main function".to_string(), Some(span));
        reporter.report(diag);
        return Err(reporter.format_all());
    }

    // Generate code
    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta");

    let mut mirs = Vec::new();
    for ast in &const_evaluated_asts {
        if let AstNode::FuncDef { name, .. } = ast {
            let mir = resolver.lower_to_mir(ast);
            mirs.push(mir);
        }
    }

    codegen.gen_mirs(&mirs);

    let ee = codegen.finalize_and_jit("native").map_err(|e| {
        let span = SourceSpan::single(SourceLocation::new(filename, 1, 1, 0));
        let diag = diagnostic_from_code(
            "E4002",
            format!("Code generation failed: {}", e),
            Some(span),
        );
        reporter.report(diag);
        reporter.format_all()
    })?;

    // Map runtime functions (same as before)
    // ... (runtime function mapping code would go here)

    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee.get_function::<MainFn>("main").map_err(|_| {
            let span = SourceSpan::single(SourceLocation::new(filename, 1, 1, 0));
            let diag = diagnostic_from_code(
                "E4001",
                "Main function not found in generated code".to_string(),
                Some(span),
            );
            reporter.report(diag);
            reporter.format_all()
        })?;
        Ok(main.call())
    }
}
