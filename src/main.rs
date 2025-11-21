// src/main.rs
use zeta::{parse_zeta, Resolver, LLVMCodegen};
use inkwell::context::Context;
use std::error::Error;

pub fn compile_and_run_zeta(input: &str) -> Result<i32, Box<dyn Error>> {
    let (_, asts) = parse_zeta(input).map_err(|e| format!("Parse error: {:?}", e))?;
    let mut resolver = Resolver::new(); // auto-registers Addable for i32

    for ast in &asts {
        resolver.register(ast.clone());
    }

    if !resolver.typecheck(&asts) {
        return Err("Typecheck/borrowck failed".into());
    }

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta_module");

    let main_func = asts.iter().find(|ast| {
        if let AstNode::FuncDef { name, .. } = ast {
            name == "main"
        } else {
            false
        }
    }).ok_or("No main function")?;

    let mut mir = resolver.lower_to_mir(main_func);
    resolver.fold_semiring_chains(&mut mir);
    codegen.gen_mir(&mir);

    let ee = codegen.finalize_and_jit()?;

    type MainFn = unsafe extern "C" fn() -> i32;
    unsafe {
        let main = ee.get_function::<MainFn>("main").map_err(|_| "No main")?;
        Ok(main.call())
    }
}

fn main() {
    let code = r#"
fn main() -> i32 {
    let a = 1;
    let b = 2;
    let c = 3;
    let d = 4;
    let e = 5;
    let f = 6;
    let g = 7;
    let h = 8;
    a.add(b).add(c).add(d).add(e).add(f).add(g).add(h)
}
"#;

    match compile_and_run_zeta(code) {
        Ok(res) => println!("Result: {}", res),
        Err(e) => eprintln!("Error: {}", e),
    }
}
