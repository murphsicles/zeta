// src/lib.rs
pub mod ast;
pub mod borrow;
pub mod codegen;
pub mod mir;
pub mod parser;
pub mod resolver;
pub mod specialization;
pub mod actor;
pub mod std;

pub use parser::parse_zeta;
pub use resolver::Resolver;
pub use codegen::LLVMCodegen;
pub use actor::{init_runtime, spawn};

use inkwell::context::Context;
use std::error::Error;

pub fn compile_and_run_zeta(code: &str) -> Result<i64, Box<dyn Error>> {
    actor::init_runtime();

    let (_, asts) = parse_zeta(code).map_err(|e| format!("Parse error: {:?}", e))?;

    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    resolver.typecheck(&asts);

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "bench");

    let main_func = asts
        .iter()
        .find(|a| matches!(a, AstNode::FuncDef { name, .. } if name == "main"))
        .ok_or("No main function")?;

    let mir = resolver.lower_to_mir(main_func);
    codegen.gen_mir(&mir);
    let ee = codegen.finalize_and_jit()?;

    unsafe {
        let free_fn = std::std_free as *const () as usize;
        ee.add_global_mapping(&codegen.module.get_function("free").unwrap(), free_fn);
    }

    type MainFn = unsafe extern "C" fn() -> i64;
    unsafe {
        let main = ee
            .get_function::<MainFn>("main")
            .map_err(|_| "No main")?;
        Ok(main.call())
    }
}
