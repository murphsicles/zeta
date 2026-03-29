// src/lib.rs
//! # Zeta Compiler Library (v0.3.4 Foundation Release)
//!
//! The core library for the Zeta systems programming language.
//! This crate provides a complete pipeline:
//!   1. Parsing → AST
//!   2. Resolution + monomorphization + MIR lowering
//!   3. LLVM code generation + JIT/AOT
//!
//! This is the **foundational bedrock** for all future Zeta self-hosting.

#![allow(clippy::missing_safety_doc)]
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

    let (_, asts) = parse_zeta(code).map_err(|e| format!("Parse error: {:?}", e))?;

    // DEBUG: Print ASTs
    println!("[DEBUG] Parsed {} ASTs", asts.len());
    for (i, ast) in asts.iter().enumerate() {
        println!("[DEBUG] AST {}: {:?}", i, ast);
    }

    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    if !resolver.typecheck(&asts) {
        return Err("Typecheck failed".into());
    }

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta");

    // Generate MIR for all function definitions, not just main
    let mut mirs = Vec::new();
    for ast in &asts {
        if let AstNode::FuncDef { name, .. } = ast {
            let mir = resolver.lower_to_mir(ast);
            mirs.push(mir);
            println!("[DEBUG] Generated MIR for function: {}", name);
        }
    }

    // Check if we have a main function (for backward compatibility with tests)
    let has_main = asts
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

    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee
            .get_function::<MainFn>("main")
            .map_err(|_| "No main".to_string())?;
        Ok(main.call())
    }
}
