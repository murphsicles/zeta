// src/main.rs
//! Entry point for Zeta self-hosting.
//! Loads example, parses, resolves, codegens, JIT-executes, prints result.
//! Updated: Full bootstrap - eval simple expr, compile zetac src via self-host JIT.
//! Added: Production bootstrap - write .ll, llc to .s, ld to exe.
//! Added: REPL mode: if no args, loop read line, parse expr, eval, print.
//! Added: AI code gen templates: use XAI for scaffolds in REPL.
//! Added: Cross-platform: WASM if --wasm flag.

use inkwell::context::Context;
use std::fs;
use std::io::{self, BufRead};
use zetac::ast::AstNode;
use zetac::{LLVMCodegen, Resolver, actor, parse_zeta};
use zetac::xai::XAIClient;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize async actor runtime.
    actor::init_runtime();

    let args: Vec<String> = std::env::args().collect();
    let wasm = args.contains(&"--wasm".to_string());
    if args.len() > 1 && !args[1].starts_with("--") {
        // File mode
        let code = fs::read_to_string(&args[1])?;
        let (_, asts) = parse_zeta(&code).map_err(|e| format!("Parse error: {:?}", e))?;

        // Print parsed nodes count for test.
        println!("Parsed {} nodes from {}", asts.len(), args[1]);

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
                    let mir = resolver.lower_to_mir(ast);
                    resolver.dump_mir(&mir); // Visual dump for debugging
                    Some(mir)
                } else {
                    None
                }
            })
            .collect();

        // Setup LLVM.
        let context = Context::create();
        let mut codegen = LLVMCodegen::new(&context, "selfhost", wasm);
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
                    let mir = boot_resolver.lower_to_mir(ast);
                    boot_resolver.dump_mir(&mir);
                    Some(mir)
                } else {
                    None
                }
            })
            .collect();
        let boot_context = Context::create();
        let mut boot_codegen = LLVMCodegen::new(&boot_context, "bootstrap", wasm);
        boot_codegen.gen_mirs(&boot_mirs);
        let boot_ee = boot_codegen.finalize_and_jit()?;
        // Production link
        boot_codegen.link_to_exe("boot.ll", "boot.exe")?;
        println!("Bootstrap exe linked: boot.exe");
        // Execute bootstrap main (zetac's main).
        unsafe {
            let boot_main = boot_ee
                .get_function::<MainFn>("main")
                .map_err(|_| "No boot main")?;
            let boot_result = boot_main.call();
            println!("Zeta bootstrap result: {}", boot_result);
        }
    } else {
        // REPL mode or AI templates
        if let Some(ai_query) = args.get(1) {
            if ai_query == "new" {
                // AI code gen template
                let client = XAIClient::new().ok();
                if let Some(c) = client {
                    let template = c.generate_scaffold("api").ok(); // Stub
                    println!("AI scaffold: {}", template.unwrap_or_default());
                }
                return Ok(());
            }
        }
        println!("Zeta REPL (v0.0.4) - type 'exit' to quit.");
        let stdin = io::stdin();
        let mut resolver = Resolver::new();
        let mut context = Context::create();
        let mut codegen = LLVMCodegen::new(&context, "repl", wasm);
        for line in stdin.lock().lines() {
            let line = line?;
            if line.trim() == "exit" {
                break;
            }
            let (_, asts) = parse_zeta(&line).map_err(|e| format!("Parse: {:?}", e))?;
            if asts.is_empty() {
                continue;
            }
            let ast = &asts[0]; // Assume single expr
            resolver.register(ast.clone());
            resolver.typecheck(&asts);
            let mir = resolver.lower_to_mir(ast);
            resolver.dump_mir(&mir); // Visual dump
            codegen.gen_mirs(&[mir]);
            let ee = codegen.finalize_and_jit().map_err(|e| format!("JIT: {}", e))?;
            type EvalFn = unsafe extern "C" fn() -> i64;
            unsafe {
                if let Ok(eval) = ee.get_function::<EvalFn>("main") {
                    let res = eval.call();
                    println!("=> {}", res);
                } else {
                    println!("Eval failed");
                }
            }
        }
    }

    Ok(())
}
