// src/lib.rs
//! # Zeta Compiler Library (v0.3.4 Foundation Release)
//!
//! The core library for the Zeta systems programming language.
//! This crate provides a complete pipeline:
//!   1. Parsing â†’ AST
//!   2. Resolution + monomorphization + MIR lowering
//!   3. LLVM code generation + JIT/AOT
//!
//! This is the **foundational bedrock** for all future Zeta self-hosting.

#![allow(clippy::missing_safety_doc)]
#![allow(unused_variables)]
//! Every line is optimized for speed, simplicity, and clarity.

pub mod backend;
#[cfg(feature = "blockchain")]
pub mod blockchain;  // Enabled for Teranode integration
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
pub mod runtime;
pub mod std;
pub mod workflows;
pub mod zeta;
pub mod distributed;

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
        .map_err(|e| format!("Const evaluation error: {}", e))?;

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
    println!(
        "[LIB DEBUG] Got {} registered functions",
        registered_funcs.len()
    );

    for ast in registered_funcs {
        if let AstNode::FuncDef { name, .. } = &ast {
            println!("[LIB] Generating MIR for registered function: {}", name);
            let mir = resolver.lower_to_mir(&ast);
            mirs.push(mir);
        } else {
            println!("[LIB DEBUG] Registered AST is not a FuncDef: {:?}", ast);
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

    let ee = codegen.finalize_and_jit().map_err(|e| e.to_string())?;

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

    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee
            .get_function::<MainFn>("main")
            .map_err(|_| "No main".to_string())?;
        Ok(main.call())
    }
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
        .map_err(|e| {
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

    let ee = codegen.finalize_and_jit().map_err(|e| {
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
