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
pub mod frontend;
pub mod middle;
pub mod runtime;

#[cfg(test)]
mod tests;

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

    let (_, asts) = parse_zeta(code).map_err(|e| format!("Parse error: {:?}", e))?;

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

    // Generate MIR for all function definitions, not just main
    let mut mirs = Vec::new();
    for ast in &const_evaluated_asts {
        match ast {
            AstNode::FuncDef { name: _, .. } => {
                let mir = resolver.lower_to_mir(ast);
                mirs.push(mir);
            }
            AstNode::ImplBlock { body, .. } => {
                // Generate MIR for functions inside impl blocks
                for func in body {
                    if let AstNode::FuncDef { name: _, .. } = func {
                        let mir = resolver.lower_to_mir(func);
                        mirs.push(mir);
                    }
                }
            }
            _ => {}
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
    if let Some(f) = codegen.module.get_function("free") {
        let free_fn = crate::runtime::std::std_free as *const () as usize;
        ee.add_global_mapping(&f, free_fn);
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
