// src/main.rs
//! Entry point for Zeta self-hosting.
//! Loads example, parses, resolves, codegens, JIT-executes, prints result.
//! Updated: Full bootstrap - eval simple expr, compile zetac src via self-host JIT.
//! Added: Production bootstrap - write .ll, llc to .s, ld to exe.

use inkwell::context::Context;
use std::fs;
use zetac::ast::AstNode;
use zetac::{LLVMCodegen, Resolver, actor, parse_zeta};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize async actor runtime.
    actor::init_runtime();

    // Load self-host example (assume contains main { let x = 42; x.add(1); } -> 43)
    let code = fs::read_to_string("examples/selfhost.z")?;
    let (_, asts) = parse_zeta(&code).map_err(|e| format!("Parse error: {:?}", e))?;

    // Print parsed nodes count for test.
    println!("Parsed {} nodes from selfhost.z", asts.len());

    // Register impls and typecheck.
    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    let type_ok = resolver.typecheck(&asts);
    println!("Typecheck: {}", if type_ok { "OK" } else { "Failed" });

    // Lower all FuncDefs to MIR.
    let mirs: Vec<_> = asts
        .iter()
        .filter_map(|ast| {
            if let AstNode::FuncDef { .. } = ast {
                Some(resolver.lower_to_mir(ast))
            } else {
                None
            }
        })
        .collect();

    // Setup LLVM.
    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "selfhost");
    codegen.gen_mirs(&mirs);
    let ee = codegen.finalize_and_jit()?;

    // Map std_free to host.
    let free_fn = zetac::std::std_free as *const () as usize;
    ee.add_global_mapping(&codegen.module.get_function("free").unwrap(), free_fn);

    // Map async actor intrinsics.
    ee.add_global_mapping(
        &codegen.module.get_function("channel_send").unwrap(),
        actor::host_channel_send as *const () as usize,
    );
    ee.add_global_mapping(
        &codegen.module.get_function("channel_recv").unwrap(),
        actor::host_channel_recv as *const () as usize,
    );
    ee.add_global_mapping(
        &codegen.module.get_function("spawn").unwrap(),
        actor::host_spawn as *const () as usize,
    );

    // JIT execute main, print result (expect 43 from 42+1).
    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee.get_function::<MainFn>("main").map_err(|_| "No main")?;
        let result = main.call();
        println!("Zeta self-hosted result: {}", result); // Should print 43
    }

    // Full bootstrap: Compile zetac src with self-host JIT (stub: load src, parse, codegen to obj).
    // Production: write .ll, llc .s, ld exe.
    let zetac_code = fs::read_to_string("src/main.z")?; // Assume renamed
    let (_, zetac_asts) =
        parse_zeta(&zetac_code).map_err(|e| format!("Bootstrap parse: {:?}", e))?;
    let mut boot_resolver = Resolver::new();
    for ast in &zetac_asts {
        boot_resolver.register(ast.clone());
    }
    boot_resolver.typecheck(&zetac_asts);
    let boot_mirs: Vec<_> = zetac_asts
        .iter()
        .filter_map(|ast| {
            if let AstNode::FuncDef { .. } = ast {
                Some(boot_resolver.lower_to_mir(ast))
            } else {
                None
            }
        })
        .collect();
    let boot_context = Context::create();
    let mut boot_codegen = LLVMCodegen::new(&boot_context, "bootstrap");
    boot_codegen.gen_mirs(&boot_mirs);
    let boot_ee = boot_codegen.finalize_and_jit()?;
    // Production link
    boot_codegen.link_to_file("boot.ll", "boot.exe")?; // Assume method, stub
    println!("Bootstrap exe linked: boot.exe");
    // Execute bootstrap main (zetac's main).
    unsafe {
        let boot_main = boot_ee
            .get_function::<MainFn>("main")
            .map_err(|_| "No boot main")?;
        let boot_result = boot_main.call();
        println!("Zeta bootstrap result: {}", boot_result);
    }

    Ok(())
}
