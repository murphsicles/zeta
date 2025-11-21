// src/main.rs
use zeta::{parse_zeta, Resolver, LLVMCodegen};
use inkwell::context::Context;
use std::error::Error;

pub fn compile_and_run_zeta(input: &str) -> Result<i64, Box<dyn Error>> {
    let (_, asts) = parse_zeta(input).map_err(|e| format!("Parse error: {:?}", e))?;
    let mut resolver = Resolver::new();

    for ast in &asts {
        resolver.register(ast.clone());
    }

    if !resolver.typecheck(&asts) {
        return Err("Typecheck/borrowck failed".into());
    }

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta_module");

    let main_func = asts.iter()
        .find(|a| matches!(a, AstNode::FuncDef { name, .. } if name == "main"))
        .ok_or("No main function")?;

    let mut mir = resolver.lower_to_mir(main_func);
    resolver.fold_semiring_chains(&mut mir);
    codegen.gen_mir(&mir);

    let ee = codegen.finalize_and_jit()?;

    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee.get_function::<MainFn>("main").map_err(|_| "No main")?;
        Ok(main.call())
    }
}

fn main() {
    let code = r#"
fn main() -> i64 {
    datetime_now()
}
"#;

    match compile_and_run_zeta(code) {
        Ok(res) => println!("Result: {res}"),
        Err(e) => eprintln!("Error: {e}"),
    }
}
