use zeta::{parse_zeta, Resolver, LLVMCodegen};
use inkwell::context::Context;
use std::error::Error;

pub fn compile_and_run_zeta(input: &str) -> Result<i32, Box<dyn Error>> {
    let (_, asts) = parse_zeta(input).map_err(|e| format!("Parse error: {:?}", e))?;
    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    if !resolver.typecheck(&asts) {
        return Err("Typecheck failed".into());
    }

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta_module");
    codegen.gen_intrinsics();

    for ast in &asts {
        codegen.gen_func(ast, &resolver);
    }

    let ee = codegen.finalize_and_jit()?;

    unsafe {
        type MainFn = unsafe extern "C" fn() -> i32;
        if let Some(main) = codegen.get_fn::<MainFn>("main") {
            Ok(main.call())
        } else {
            Err("No main function found".into())
        }
    }
}

fn main() {
    let code = r#"
fn add(a: i32, b: i32) -> i32 {
    a + b
}

concept Addable {
    fn add(self: Self, rhs: Self) -> Self;
}

impl Addable for i32 {}

fn main() -> i32 {
    let x = 10;
    let y = 20;
    x.add(y)
}
"#;

    match compile_and_run_zeta(code) {
        Ok(res) => println!("Result: {}", res),
        Err(e) => eprintln!("Error: {}", e),
    }
}
