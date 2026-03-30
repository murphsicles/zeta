// Debug monomorphization
use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::resolver::Resolver;
use zetac::middle::specialization::MonoKey;

fn main() {
    println!("=== Debugging Monomorphization ===");

    // Simple generic function
    let code = r#"
    fn identity<T>(x: T) -> T { x }
    
    fn main() -> i64 {
        identity::<i64>(42)
    }
    "#;

    println!("Parsing code...");
    let result = parse_zeta(code);
    match result {
        Ok((remaining, asts)) => {
            println!("Parse successful, remaining: '{}'", remaining);
            println!("AST count: {}", asts.len());

            let mut resolver = Resolver::new();

            println!("\nRegistering ASTs...");
            for ast in &asts {
                resolver.register(ast.clone());
            }

            println!("\nType checking...");
            let type_ok = resolver.typecheck(&asts);
            println!("Type check result: {}", type_ok);

            if type_ok {
                println!("\nChecking for generic functions...");
                // Look for identity function
                for ast in &asts {
                    if let zetac::frontend::ast::AstNode::FuncDef { name, .. } = ast {
                        println!("Found function: {}", name);
                        if name == "identity" {
                            println!("Found identity function!");

                            // Try to monomorphize
                            let key = MonoKey {
                                func_name: "identity".to_string(),
                                type_args: vec!["i64".to_string()],
                            };

                            println!("\nMonomorphizing with key: {:?}", key);
                            let mono_ast = resolver.monomorphize(key.clone(), ast);
                            println!("Monomorphized AST: {:?}", mono_ast);

                            let mono_mir = resolver.lower_to_mir(&mono_ast);
                            println!("Monomorphized MIR name: {:?}", mono_mir.name);
                            println!("Monomorphized MIR has {} statements", mono_mir.stmts.len());

                            // Check if the MIR has type_args in calls
                            for stmt in &mono_mir.stmts {
                                println!("Statement: {:?}", stmt);
                            }
                        }
                    }
                }
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}
