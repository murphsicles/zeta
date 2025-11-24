// src/main.rs
//! Entry point for Zeta self-hosting.
//! Loads example, parses, resolves, codegens, JIT-executes, prints result.

use inkwell::context::Context;
use std::fs;
use zetac::ast::AstNode;
use zetac::{LLVMCodegen, Resolver, actor, parse_zeta};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize actor runtime.
    actor::init_runtime();

    // Load self-host example.
    let code = fs::read_to_string("examples/selfhost.zeta")?;
    let (_, asts) = parse_zeta(&code).map_err(|e| format!("Parse error: {:?}", e))?;

    // Register impls and typecheck.
    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    resolver.typecheck(&asts);

    // Setup LLVM.
    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "selfhost");

    // Find and lower main func to MIR.
    let main_func = asts
        .iter()
        .find(|a| matches!(a, AstNode::FuncDef { name, .. } if name == "main"))
        .ok_or("No main function")?;

    let mir = resolver.lower_to_mir(main_func);
    codegen.gen_mir(&mir);
    let ee = codegen.finalize_and_jit()?;

    // Map std_free to host.
    let free_fn = zetac::std::std_free as *const () as usize;
    ee.add_global_mapping(&codegen.module.get_function("free").unwrap(), free_fn);

    // JIT execute main, print result.
    type MainFn = unsafe extern "C" fn() -> i32;
    unsafe {
        let main = ee.get_function::<MainFn>("main").map_err(|_| "No main")?;
        let result = main.call();
        println!("Zeta 1.0 self-hosted result: {}", result);
        Ok(())
    }
}
