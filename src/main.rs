// src/main.rs
//! Entry point for the Zeta compiler.
//! Demonstrates self-hosting by parsing, resolving, generating code, JIT-executing example code, and bootstrapping.
use inkwell::context::Context;
use std::collections::HashMap;
use std::fs;
use std::io::{self, BufRead, Write};
use zetac::backend::codegen::codegen::LLVMCodegen;
use zetac::frontend::ast::AstNode;
use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::mir::mir::Mir;
use zetac::middle::resolver::resolver::Resolver;
use zetac::middle::specialization::{is_cache_safe, lookup_specialization, record_specialization};
use zetac::runtime::actor::channel;
use zetac::runtime::actor::scheduler;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize async actor runtime.
    scheduler::init_runtime();

    let args: Vec<String> = std::env::args().collect();
    let dump_mir = args.iter().any(|a| a == "--dump-mir");

    if args.len() > 1 && args[1] == "--repl" {
        repl(dump_mir)?;
        return Ok(());
    }

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

    // Collect used specializations
    let used_specs = resolver.collect_used_specializations(&asts);

    // Lower all FuncDefs to MIR.
    let mir_map: HashMap<String, Mir> = asts
        .iter()
        .filter_map(|ast| {
            if let AstNode::FuncDef { name, .. } = ast {
                Some((name.clone(), resolver.lower_to_mir(ast)))
            } else {
                None
            }
        })
        .collect();

    // Monomorphize: for each generic fn used, duplicate MIR with mangled names
    let mut mono_mirs: Vec<Mir> = vec![];

    for (fn_name, specs) in &used_specs {
        if let Some(base_mir) = mir_map.get(fn_name) {
            if specs.is_empty() {
                mono_mirs.push(base_mir.clone());
            } else {
                for spec in specs {
                    let key = zetac::middle::specialization::MonoKey {
                        func_name: fn_name.clone(),
                        type_args: spec.clone(),
                    };
                    let mangled = key.mangle();
                    // Check persistent + in-memory cache first
                    if lookup_specialization(&key).is_none() {
                        let cache_safe = spec.iter().all(|t| is_cache_safe(t));
                        record_specialization(
                            key.clone(),
                            zetac::middle::specialization::MonoValue {
                                llvm_func_name: mangled.clone(),
                                cache_safe,
                            },
                        );
                    }
                    let mut mono_mir = base_mir.clone();
                    mono_mir.name = Some(mangled);
                    mono_mirs.push(mono_mir);
                }
            }
        }
    }
    // Add non-generic mirs not in used_specs
    for (fn_name, mir) in &mir_map {
        if !used_specs.contains_key(fn_name) {
            mono_mirs.push(mir.clone());
        }
    }

    if dump_mir {
        for mir in &mono_mirs {
            if let Some(name) = &mir.name {
                println!("=== MIR for {name} ===");
                println!("{mir:#?}");
            }
        }
    }

    // Setup LLVM.
    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "selfhost");
    codegen.gen_mirs(&mono_mirs);
    let ee = codegen.finalize_and_jit()?;

    // Map std_free to host.
    let free_fn = zetac::runtime::std::std_free as *const () as usize;
    ee.add_global_mapping(&codegen.module.get_function("free").unwrap(), free_fn);
    // Map async actor intrinsics.
    ee.add_global_mapping(
        &codegen.module.get_function("channel_send").unwrap(),
        channel::host_channel_send as *const () as usize,
    );
    ee.add_global_mapping(
        &codegen.module.get_function("channel_recv").unwrap(),
        channel::host_channel_recv as *const () as usize,
    );
    ee.add_global_mapping(
        &codegen.module.get_function("spawn").unwrap(),
        scheduler::host_spawn as *const () as usize,
    );

    // JIT execute main, print result (expect 43 from 42+1).
    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee.get_function::<MainFn>("main").map_err(|_| "No main")?;
        let result = main.call();
        println!("Zeta self-hosted result: {}", result); // Should print 43
    }

    // Full bootstrap stub (unchanged – persistence works via Resolver::drop)
    let zetac_code = fs::read_to_string("examples/zetac.z")?; // Placeholder
    let (_, zetac_asts) =
        parse_zeta(&zetac_code).map_err(|e| format!("Bootstrap parse: {:?}", e))?;
    let mut boot_resolver = Resolver::new();
    for ast in &zetac_asts {
        boot_resolver.register(ast.clone());
    }
    boot_resolver.typecheck(&zetac_asts);
    let boot_used_specs = boot_resolver.collect_used_specializations(&zetac_asts);
    let boot_mir_map: HashMap<String, Mir> = zetac_asts
        .iter()
        .filter_map(|ast| {
            if let AstNode::FuncDef { name, .. } = ast {
                Some((name.clone(), boot_resolver.lower_to_mir(ast)))
            } else {
                None
            }
        })
        .collect();

    let mut boot_mono_mirs: Vec<Mir> = vec![];

    for (fn_name, specs) in &boot_used_specs {
        if let Some(base_mir) = boot_mir_map.get(fn_name) {
            if specs.is_empty() {
                boot_mono_mirs.push(base_mir.clone());
            } else {
                for spec in specs {
                    let key = zetac::middle::specialization::MonoKey {
                        func_name: fn_name.clone(),
                        type_args: spec.clone(),
                    };
                    let mangled = key.mangle();
                    if lookup_specialization(&key).is_none() {
                        let cache_safe = spec.iter().all(|t| is_cache_safe(t));
                        record_specialization(
                            key.clone(),
                            zetac::middle::specialization::MonoValue {
                                llvm_func_name: mangled.clone(),
                                cache_safe,
                            },
                        );
                    }
                    let mut mono_mir = base_mir.clone();
                    mono_mir.name = Some(mangled);
                    boot_mono_mirs.push(mono_mir);
                }
            }
        }
    }
    for (fn_name, mir) in &boot_mir_map {
        if !boot_used_specs.contains_key(fn_name) {
            boot_mono_mirs.push(mir.clone());
        }
    }

    if dump_mir {
        for mir in &boot_mono_mirs {
            if let Some(name) = &mir.name {
                println!("=== Bootstrap MIR for {name} ===");
                println!("{mir:#?}");
            }
        }
    }

    let boot_context = Context::create();
    let mut boot_codegen = LLVMCodegen::new(&boot_context, "bootstrap");
    boot_codegen.gen_mirs(&boot_mono_mirs);
    let boot_ee = boot_codegen.finalize_and_jit()?;
    unsafe {
        let boot_main = boot_ee
            .get_function::<MainFn>("main")
            .map_err(|_| "No boot main")?;
        let boot_result = boot_main.call();
        println!("Zeta bootstrap result: {}", boot_result);
    }
    Ok(())
}

fn repl(dump_mir: bool) -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin();
    let mut stdin_lock = stdin.lock();
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut line = String::new();
        stdin_lock.read_line(&mut line)?;
        let line = line.trim();

        if line.is_empty() {
            continue;
        }

        let code = format!("fn main() -> i64 {{ {} }}", line);
        let (_, asts) = parse_zeta(&code).map_err(|e| format!("Parse error: {:?}", e))?;

        if asts.is_empty() {
            continue;
        }

        let mut resolver = Resolver::new();
        for ast in &asts {
            resolver.register(ast.clone());
        }

        let type_ok = resolver.typecheck(&asts);
        if !type_ok {
            println!("Typecheck failed");
            continue;
        }

        let mir_map: HashMap<String, Mir> = asts
            .iter()
            .filter_map(|ast| {
                if let AstNode::FuncDef { name, .. } = ast {
                    Some((name.clone(), resolver.lower_to_mir(ast)))
                } else {
                    None
                }
            })
            .collect();

        if dump_mir {
            if let Some(main_mir) = mir_map.get("main") {
                println!("=== REPL MIR for main ===");
                println!("{main_mir:#?}");
            }
        }

        let context = Context::create();
        let mut codegen = LLVMCodegen::new(&context, "repl");
        codegen.gen_mirs(&mir_map.values().cloned().collect::<Vec<_>>());
        let ee = codegen.finalize_and_jit()?;

        let free_fn = zetac::runtime::std::std_free as *const () as usize;
        ee.add_global_mapping(&codegen.module.get_function("free").unwrap(), free_fn);
        ee.add_global_mapping(
            &codegen.module.get_function("channel_send").unwrap(),
            channel::host_channel_send as *const () as usize,
        );
        ee.add_global_mapping(
            &codegen.module.get_function("channel_recv").unwrap(),
            channel::host_channel_recv as *const () as usize,
        );
        ee.add_global_mapping(
            &codegen.module.get_function("spawn").unwrap(),
            scheduler::host_spawn as *const () as usize,
        );

        type ReplFn = unsafe extern "C" fn() -> i64;
        unsafe {
            if let Ok(f) = ee.get_function::<ReplFn>("main") {
                let res = f.call();
                println!("{}", res);
            } else {
                println!("No main function");
            }
        }
    }
}
