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
//! Every line is optimized for speed, simplicity, and clarity.

pub mod backend;
pub mod frontend;
pub mod middle;
pub mod runtime;

pub use backend::codegen::LLVMCodegen;
pub use frontend::ast::AstNode;
pub use frontend::parser::top_level::parse_zeta;
pub use middle::mir::mir::Mir;
pub use middle::resolver::resolver::Resolver;
pub use runtime::actor::scheduler::{init_runtime, spawn};

use inkwell::context::Context;

/// Compiles a Zeta source string to executable code and runs `main()`.
///
/// This is the primary public API used by the bootstrap `main.rs` and will
/// become the entry point when Zeta becomes fully self-hosted.
pub fn compile_and_run_zeta(code: &str) -> Result<i64, String> {
    init_runtime();

    let (_, asts) = parse_zeta(code).map_err(|e| format!("Parse error: {:?}", e))?;

    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    if !resolver.typecheck(&asts) {
        return Err("Typecheck failed".into());
    }

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta");

    let main_func = asts
        .iter()
        .find(|a| matches!(a, AstNode::FuncDef { name, .. } if name == "main"))
        .ok_or("No main function".to_string())?;

    let mir = resolver.lower_to_mir(main_func);
    codegen.gen_mirs(&[mir]);

    let ee = codegen.finalize_and_jit().map_err(|e| e.to_string())?;

    // Map required runtime functions
    if let Some(f) = codegen.module.get_function("free") {
        let free_fn = crate::runtime::std::std_free as *const () as usize;
        ee.add_global_mapping(&f, free_fn);
    }

    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee
            .get_function::<MainFn>("main")
            .map_err(|_| "No main".to_string())?;
        Ok(main.call())
    }
}
