// src/lib.rs
//! Zeta compiler library crate.
//! Exports modules and utilities for parsing, type resolution, code generation, and runtime execution.
pub mod backend;
pub mod frontend;
pub mod middle;
pub mod runtime;
pub use backend::codegen::codegen::LLVMCodegen;
pub use frontend::ast::AstNode;
pub use frontend::parser::top_level::parse_zeta;
use inkwell::context::Context;
pub use middle::mir::mir::Mir;
pub use middle::resolver::resolver::Resolver;
pub use runtime::actor::scheduler::{init_runtime, spawn};
/// Compiles Zeta source code to LLVM IR, JIT-executes it, and returns the result from main.
pub fn compile_and_run_zeta(code: &str) -> Result<i64, String> {
    // Init runtime.
    init_runtime();
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
        .find(|a| {
            if let AstNode::FuncDef { name, .. } = a {
                name == "main"
            } else {
                false
            }
        })
        .ok_or("No main function".to_string())?;
    let mir = resolver.lower_to_mir(main_func);
    codegen.gen_mirs(&[mir]);
    let ee = codegen.finalize_and_jit().map_err(|e| e.to_string())?;
    // Map std_free.
    {
        let free_fn = crate::runtime::std::std_free as *const () as usize;
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
