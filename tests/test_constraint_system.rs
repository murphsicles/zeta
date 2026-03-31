use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::new_resolver::InferContext;

fn main() {
    println!("Testing constraint system for Vec::<i32>::new()");
    
    let code = r#"
    fn main() -> i64 {
        let v = lt(Vec, i32)::new();
        0
    }
    "#;
    
    // Parse the code
    let result = parse_zeta(code);
    match result {
        Ok((remaining, ast)) => {
            println!("Parsed successfully");
            println!("Remaining input: '{}'", remaining);
            println!("AST: {:?}", ast);
            
            // Create inference context
            let mut ctx = InferContext::new();
            
            // Type check
            for node in &ast {
                match ctx.infer(node) {
                    Ok(ty) => println!("Node type: {:?}", ty),
                    Err(e) => println!("Type error: {}", e),
                }
            }
            
            // Solve constraints
            match ctx.solve() {
                Ok(_) => println!("Constraints solved successfully"),
                Err(errors) => {
                    println!("Constraint solving failed with {} errors:", errors.len());
                    for error in errors {
                        println!("  {:?}", error);
                    }
                }
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}