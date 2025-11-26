// src/lib.rs
//! Zeta compiler library.
//! Exports core components for parsing, resolving, codegen, and runtime.
//! Enable "codegen" feature for LLVM JIT support.

#[cfg(feature = "codegen")]
pub mod codegen;
pub mod actor;
pub mod ast;
pub mod borrow;
#[cfg(feature = "codegen")]
pub mod mir;
pub mod parser;
pub mod resolver;
pub mod specialization;
pub mod std;

#[cfg(feature = "codegen")]
pub use codegen::LLVMCodegen;
pub use actor::{init_runtime, spawn};
pub use parser::parse_zeta;
pub use resolver::Resolver;

use crate::ast::AstNode;
#[cfg(feature = "codegen")]
use inkwell::context::Context;

/// Compiles and JIT-executes Zeta code to i64 result (requires "codegen" feature).
#[cfg(feature = "codegen")]
pub fn compile_and_run_zeta(code: &str) -> Result<i64, String> {
    // Init runtime.
    crate::actor::init_runtime();

    // Parse to AST.
    let (_, asts) = parse_zeta(code).map_err(|e| format!("Parse error: {:?}", e))?;

    // Resolve and check.
    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    resolver.typecheck(&asts);

    // LLVM setup.
    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "bench");

    // Lower main to MIR.
    let main_func = asts
        .iter()
        .find(|a| matches!(a, AstNode::FuncDef { name, .. } if name == "main"))
        .ok_or("No main function".to_string())?;

    let mir = resolver.lower_to_mir(main_func);
    codegen.gen_mir(&mir);
    let ee = codegen.finalize_and_jit().map_err(|e| e.to_string())?;

    // Map std_free.
    {
        let free_fn = crate::std::std_free as *const () as usize;
        ee.add_global_mapping(&codegen.module.get_function("free").unwrap(), free_fn);
    }

    // Execute.
    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee
            .get_function::<MainFn>("main")
            .map_err(|_| "No main".to_string())?;
        Ok(main.call())
    }
}

/// Stub without codegen: returns error.
#[cfg(not(feature = "codegen"))]
pub fn compile_and_run_zeta(_code: &str) -> Result<i64, String> {
    Err("LLVM codegen requires 'codegen' feature".to_string())
}
