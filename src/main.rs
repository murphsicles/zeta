// src/main.rs
use zeta::{parse_zeta, Resolver, LLVMCodegen, actor};
use inkwell::context::Context;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize M:N actor runtime on startup
    actor::init_runtime();

    let code = fs::read_to_string("examples/selfhost.zeta")?;
    let (_, asts) = parse_zeta(&code).map_err(|e| format!("Parse error: {:?}", e))?;

    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    resolver.typecheck(&asts);

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "selfhost");

    let main_func = asts.iter()
        .find(|a| matches!(a, AstNode::FuncDef { name, .. } if name == "main"))
        .ok_or("No main function")?;

    let mir = resolver.lower_to_mir(main_func);
    codegen.gen_mir(&mir);
    let ee = codegen.finalize_and_jit()?;

    // Add std::free mapping
    unsafe {
        let free_fn = zeta::std::free as usize;
        ee.add_global_mapping(&codegen.module.get_function("free").unwrap(), free_fn);
    }

    type MainFn = unsafe extern "C" fn() -> i32;
    unsafe {
        let main = ee.get_function::<MainFn>("main").map_err(|_| "No main")?;
        let result = main.call();
        println!("Zeta 1.0 self-hosted result: {}", result);
    }
    Ok(())
}
