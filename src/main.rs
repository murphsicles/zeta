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

    // Lower all FuncDefs to MIR.
    let mirs: Vec<_> = asts.iter().filter_map(|ast| {
        if let AstNode::FuncDef { .. } = ast {
            Some(resolver.lower_to_mir(ast))
        } else {
            None
        }
    }).collect();

    // Setup LLVM.
    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "selfhost");
    codegen.gen_mirs(&mirs);
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
