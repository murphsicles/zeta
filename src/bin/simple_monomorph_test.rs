// Simple test to check if monomorphization works
// This bypasses the integration module which has compilation errors

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::resolver::Resolver;

fn main() {
    println!("=== Simple Monomorphization Test ===");

    // Very simple test: identity function
    let code = r#"
    fn identity<T>(x: T) -> T { x }
    
    fn main() -> i64 {
        identity::<i64>(42)
    }
    "#;

    println!("Parsing code...");
    match parse_zeta(code) {
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
                println!("\nGenerating MIR...");
                let mirs: Vec<_> = asts
                    .iter()
                    .map(|ast| {
                        if let zetac::frontend::ast::AstNode::FuncDef { name, .. } = ast {
                            println!("Generating MIR for function: {}", name);
                        }
                        resolver.lower_to_mir(ast)
                    })
                    .collect();

                println!("Generated {} MIRs", mirs.len());

                // Check if identity function is in MIRs
                for mir in &mirs {
                    if let Some(name) = &mir.name {
                        println!("MIR name: {}", name);
                        if name == "identity" {
                            println!("Found identity MIR!");
                            println!("It has {} statements", mir.stmts.len());
                            for stmt in &mir.stmts {
                                println!("  Statement: {:?}", stmt);
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
