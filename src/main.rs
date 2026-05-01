// src/main.rs
//! # Zeta Compiler Entry Point (v0.3.4 Foundation Release)
//!
//! This is the final Rust bootstrap for Zeta.
//! It drives the complete pipeline:
//! • Parse → AST
//! • Resolve + monomorphize + typecheck
//! • Lower to MIR
//! • LLVM codegen + JIT/AOT
//!
//! This file is the last Rust entry point before full self-hosting.

use inkwell::context::Context;
use std::collections::HashMap;
use std::fs;
use std::io::{self, BufRead, Write};
use std::path::Path;

use zetac::backend::codegen::LLVMCodegen;
use zetac::backend::codegen::finalize_and_aot;
use zetac::diagnostics::{Diagnostic, DiagnosticReporter, SourceLocation, SourceSpan, Severity};
use zetac::frontend::ast::AstNode;
use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::mir::mir::Mir;
use zetac::middle::resolver::resolver::Resolver;
use zetac::middle::specialization::{
    MonoKey, is_cache_safe, lookup_specialization, record_specialization,
};
use zetac::runtime::actor::scheduler;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    scheduler::init_runtime();

    let args: Vec<String> = std::env::args().collect();
    let dump_mir = args.iter().any(|a| a == "--dump-mir");

    if args.len() > 1 && args[1] == "--repl" {
        return repl(dump_mir);
    }

    let mut input = None;
    let mut output = args.iter().position(|a| a == "-o").and_then(|i| args.get(i+1)).cloned();
    let mut i = 1;

    if args.iter().any(|a| a == "--bootstrap") {
        return bootstrap_zeta(&output);
    }

    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                if i < args.len() {
                    output = Some(args[i].clone());
                }
            }
            "--dump-mir" => {}
            _ => input = Some(args[i].clone()),
        }
        i += 1;
    }
    if let Some(file) = input {
        let code = fs::read_to_string(&file)?;
        // Strip UTF-8 BOM if present
        let code = code.trim_start_matches('\u{FEFF}');
        let result = parse_zeta(code);
        match result {
            Ok((_remaining, asts)) => {
                // Run CTFE evaluation on parsed ASTs
                let asts = match zetac::middle::const_eval::evaluate_constants(&asts) {
                    Ok(ctfe_asts) => {
                        // After CTFE, filter out comptime-only function definitions
                        // (they've been evaluated and are no longer needed for codegen)
                        // const fns are kept — they may still be needed at runtime if
                        // CTFE couldn't fully inline all call sites.
                        let runtime_asts: Vec<_> = ctfe_asts
                            .into_iter()
                            .filter(|ast| {
                                !matches!(
                                    ast,
                                    AstNode::FuncDef {
                                        comptime_: true,
                                        ..
                                    }
                                )
                            })
                            .collect();
                        runtime_asts
                    }
                    Err(e) => {
                        zetac::diag_warning!("W0001", "CTFE warning (non-fatal): {}", e);
                        asts
                    }
                };

                let mut resolver = Resolver::new();

                // Expand macros before registration
                let expanded_asts = match resolver.expand_macros(&asts) {
                    Ok(ea) => ea,
                    Err(e) => {
                        zetac::diag_warning!("W0002", "Macro expansion warning (non-fatal): {}", e);
                        asts.clone()
                    }
                };

                for ast in &expanded_asts {
                    resolver.register(ast.clone());
                }

                // Use expanded ASTs for typechecking
                let typecheck_asts = &expanded_asts;

                let type_ok = resolver.typecheck(&expanded_asts);
                if !type_ok {
                    let diag = Diagnostic {
                        severity: Severity::Warning,
                        code: Some("W0003".to_string()),
                        message: "Typecheck failed (non-fatal) — proceeding with unresolved types.".to_string(),
                        span: None,
                        context: None,
                        help: None,
                        note: Some("The program may have type errors that will manifest at runtime.".to_string()),
                        suggestions: vec![],
                    };
                    eprintln!("{}", diag.format(None));
                }

                let func_asts = resolver.get_registered_funcs();
                let mir_map: HashMap<String, Mir> = func_asts
                    .iter()
                    .filter_map(|ast| {
                        if let AstNode::FuncDef { name, .. } = ast {
                            Some((name.clone(), resolver.lower_to_mir(ast)))
                        } else {
                            None
                        }
                    })
                    .collect();

                let mut used_specs = resolver.collect_used_specializations(&asts);

                // Ensure add<i64> specialization exists
                let add_key = MonoKey {
                    func_name: "add".to_string(),
                    type_args: vec!["i64".to_string()],
                };
                if lookup_specialization(&add_key).is_none() {
                    record_specialization(
                        add_key.clone(),
                        zetac::middle::specialization::MonoValue {
                            llvm_func_name: add_key.mangle(),
                            cache_safe: true,
                        },
                    );
                }
                used_specs
                    .entry("add".to_string())
                    .or_default()
                    .push(vec!["i64".to_string()]);

                // Monomorphize everything used
                for (fn_name, specs) in &used_specs {
                    if let Some(base_ast) = func_asts.iter().find(|a| {
                        if let AstNode::FuncDef { name, .. } = a {
                            name == fn_name
                        } else {
                            false
                        }
                    }) {
                        for spec in specs {
                            let key = MonoKey {
                                func_name: fn_name.clone(),
                                type_args: spec.clone(),
                            };
                            let mono_ast = resolver.monomorphize(key.clone(), base_ast);
                            let mono_mir = resolver.lower_to_mir(&mono_ast);
                            resolver.record_mono(key, mono_mir);
                        }
                    }
                }

                // Final deduplicated MIR list
                let mut final_mirs: HashMap<String, Mir> = mir_map.clone();
                for mir in resolver.mono_mirs.values() {
                    let name = mir
                        .name
                        .as_ref()
                        .cloned()
                        .unwrap_or_else(|| "anon".to_string());
                    final_mirs.insert(name, mir.clone());
                }
                let all_mirs: Vec<Mir> = final_mirs.values().cloned().collect();

                let context = Context::create();
                let mut codegen = LLVMCodegen::new(&context, "module");
                codegen.gen_mirs(&all_mirs);

                if let Some(out) = output {
                    let obj_path = format!("{}.o", out);
                    finalize_and_aot(&codegen, Path::new(&obj_path))?;

                    // Platform-specific linking
                    let mut cmd = std::process::Command::new("gcc");
                    cmd.arg(&obj_path).arg("-o").arg(&out);

                    // Add platform-specific libraries
                    if cfg!(target_os = "windows") {
                        cmd.arg("-lmsvcrt") // Microsoft C runtime
                            .arg("-lkernel32"); // Core Windows API
                    } else {
                        // Unix/Linux/MacOS
                        cmd.arg("-lc"); // C standard library
                        cmd.arg("-no-pie"); // Needed for PIE relocation errors with generated code
                    }

                    // Add Zeta runtime library
                    // First try C runtime object file (simpler, no Rust stdlib dependencies)
                    let runtime_c_obj = std::path::Path::new("zeta_runtime_c.o");
                    let runtime_rust_obj = std::path::Path::new("zeta_runtime.o");

                    if runtime_c_obj.exists() {
                        // Use C runtime (preferred - no Rust stdlib dependencies)
                        cmd.arg(runtime_c_obj);
                    } else if runtime_rust_obj.exists() {
                        // Fallback to old Rust runtime object
                        cmd.arg(runtime_rust_obj);
                    } else {
                        // Check for compiled libraries
                        let runtime_lib_windows =
                            std::path::Path::new("runtime_lib/target/release/zeta_runtime.lib");
                        let runtime_lib_unix = std::path::Path::new("libzeta.a");

                        if runtime_lib_windows.exists() {
                            // Windows: link against .lib file
                            cmd.arg(runtime_lib_windows);
                        } else if runtime_lib_unix.exists() {
                            // Unix: link against .a file
                            cmd.arg(runtime_lib_unix);
                        }
                    }

                    let status = cmd.status()?;

                    if !status.success() {
                        return Err("Linking failed".into());
                    }
                    println!("Compiled to {}", out);
                } else {
                    let ee = codegen.finalize_and_jit()?;
                    type MainFn = unsafe extern "C" fn() -> i64;
                    unsafe {
                        if let Ok(main) = ee.get_function::<MainFn>("main") {
                            let result = main.call();
                            println!("Result: {}", result);
                        } else {
                            // Find what functions exist
                            let func_names: Vec<String> = mir_map.keys().cloned().collect();
                            let diag = Diagnostic {
                                severity: Severity::Error,
                                code: Some("E0001".to_string()),
                                message: format!("No main function found. Available: [{}]", func_names.join(", ")),
                                span: None,
                                context: None,
                                help: Some("Define a function named 'main' that returns i64.".to_string()),
                                note: None,
                                suggestions: vec![],
                            };
                            eprintln!("{}", diag.format(None));
                        }
                    }
                }
                Ok(())
            }
            Err(e) => {
                let diag = Diagnostic {
                    severity: Severity::Error,
                    code: Some("E0100".to_string()),
                    message: format!("Parse error: {:?}", e),
                    span: None,
                    context: None,
                    help: Some("Check for syntax errors near the reported location.".to_string()),
                    note: None,
                    suggestions: vec![],
                };
                eprintln!("{}", diag.format(None));
                Err("Parse failed".into())
            }
        }
    } else {
        // Fallback self-host example
        let code = fs::read_to_string("examples/selfhost.z")?;
        let asts = parse_zeta(&code)
            .map_err(|e| format!("Parse error: {:?}", e))?
            .1; // take only owned ASTs, discard remaining slice

        let mut resolver = Resolver::new();
        for ast in &asts {
            resolver.register(ast.clone());
        }
        let _ = resolver.typecheck(&asts);

        let func_asts = resolver.get_registered_funcs();
        let mut mono_mirs = vec![];
        let used_specs = resolver.collect_used_specializations(&asts);

        for (fn_name, specs) in &used_specs {
            if let Some(base) = func_asts.iter().find(|a| {
                if let AstNode::FuncDef { name, .. } = a {
                    name == fn_name
                } else {
                    false
                }
            }) {
                for spec in specs {
                    let key = MonoKey {
                        func_name: fn_name.clone(),
                        type_args: spec.clone(),
                    };
                    let mangled = key.mangle();
                    if lookup_specialization(&key).is_none() {
                        record_specialization(
                            key.clone(),
                            zetac::middle::specialization::MonoValue {
                                llvm_func_name: mangled.clone(),
                                cache_safe: spec.iter().all(|t| is_cache_safe(t)),
                            },
                        );
                    }
                    let mono_ast = resolver.monomorphize(key.clone(), base);
                    let mut mono_mir = resolver.lower_to_mir(&mono_ast);
                    mono_mir.name = Some(mangled);
                    mono_mirs.push(mono_mir);
                }
            }
        }

        let context = Context::create();
        let mut codegen = LLVMCodegen::new(&context, "selfhost");
        codegen.gen_mirs(&mono_mirs);

        let ee = codegen.finalize_and_jit()?;
        type MainFn = unsafe extern "C" fn() -> i64;
        unsafe {
            let main = ee.get_function::<MainFn>("main")?;
            let result = main.call();
            println!("Zeta self-hosted result: {}", result);
        }
        Ok(())
    }
}

fn bootstrap_zeta(output: &Option<String>) -> Result<(), Box<dyn std::error::Error>> {
    use std::path::Path;
    fn collect(dir: &Path, files: &mut Vec<std::path::PathBuf>) -> std::io::Result<()> {
        if dir.is_dir() {
            for e in std::fs::read_dir(dir)? { let p = e?.path();
                if p.is_dir() { collect(&p, files)?; }
                else if p.extension().map_or(false, |ext| ext == "z") { files.push(p); }
            }
        }
        Ok(())
    }
    let mut funcs: std::collections::HashMap<String, AstNode> = std::collections::HashMap::new();
    let mut other: Vec<AstNode> = Vec::new();
    let mut zf = Vec::new();
    collect(Path::new("zeta_src"), &mut zf)?;
    zf.sort();
    eprintln!("Bootstrap: {} files", zf.len());
    for path in &zf {
        let code = std::fs::read_to_string(path)?;
        if let Ok((rem, asts)) = parse_zeta(&code) {
            if !rem.trim().is_empty() && rem.len() < 80 {
                eprintln!("  Partial: {} ({:?})", path.display(), &rem[..rem.len().min(40)]);
            }
            for a in asts {
                if let AstNode::FuncDef { name, .. } = &a { funcs.insert(name.clone(), a); }
                else { other.push(a); }
            }
        }
    }
    eprintln!("Parsed: {} funcs + {} items", funcs.len(), other.len());
    let mut all = other; all.extend(funcs.into_values().map(|f| f));
    let all = match zetac::middle::const_eval::evaluate_constants(&all) {
        Ok(c) => c.into_iter().filter(|a| !matches!(a, AstNode::FuncDef { comptime_: true, .. })).collect(),
        Err(e) => { eprintln!("CTFE: {}", e); all }
    };
    let mut resolver = Resolver::new();
    let all = match resolver.expand_macros(&all) {
        Ok(ea) => ea, Err(e) => { eprintln!("Macro: {}", e); all }
    };
    for ast in &all { resolver.register(ast.clone()); }
    let _ = resolver.typecheck(&all);
    let fa = resolver.get_registered_funcs();
    let mirs: Vec<Mir> = fa.iter().filter_map(|a| {
        if let AstNode::FuncDef { .. } = a { Some(resolver.lower_to_mir(a)) } else { None }
    }).collect();
    eprintln!("Lowered {} functions to MIR", mirs.len());
    let ctx = Context::create();
    let mut cg = LLVMCodegen::new(&ctx, "zeta_bootstrap");
    cg.gen_mirs(&mirs);
    if let Some(out) = output {
        let obj = format!("{}.o", out);
        finalize_and_aot(&cg, Path::new(&obj))?;
        let mut cmd = std::process::Command::new("gcc");
        cmd.arg(&obj).arg("-o").arg(&out).arg("-lc").arg("-no-pie");
        let rc = Path::new("zeta_runtime_c.o");
        if rc.exists() { cmd.arg(rc); }
        if !cmd.status()?.success() { return Err("Linking failed".into()); }
        println!("Compiled to {}", out);
    } else {
        let ee = cg.finalize_and_jit()?;
        unsafe {
            if let Ok(m) = ee.get_function::<unsafe extern "C" fn() -> i64>("main") {
                println!("Result: {}", m.call());
            } else { eprintln!("Error: no main function in bootstrap binary"); }
        }
    }
    Ok(())
}

fn repl(_dump_mir: bool) -> Result<(), Box<dyn std::error::Error>> {
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
        let asts = parse_zeta(&code)
            .map_err(|e| format!("Parse error: {:?}", e))?
            .1;

        if asts.is_empty() {
            continue;
        }

        let mut resolver = Resolver::new();
        for ast in &asts {
            resolver.register(ast.clone());
        }
        if !resolver.typecheck(&asts) {
            println!("Typecheck failed");
            continue;
        }

        let func_asts = resolver.get_registered_funcs();
        let mir_map: HashMap<String, Mir> = func_asts
            .iter()
            .filter_map(|ast| {
                if let AstNode::FuncDef { name, .. } = ast {
                    Some((name.clone(), resolver.lower_to_mir(ast)))
                } else {
                    None
                }
            })
            .collect();

        let context = Context::create();
        let mut codegen = LLVMCodegen::new(&context, "repl");
        codegen.gen_mirs(&mir_map.values().cloned().collect::<Vec<_>>());

        let ee = codegen.finalize_and_jit()?;
        type ReplFn = unsafe extern "C" fn() -> i64;
        unsafe {
            if let Ok(f) = ee.get_function::<ReplFn>("main") {
                println!("{}", f.call());
            } else {
                eprintln!("REPL: no main function");
            }
        }
    }
}
