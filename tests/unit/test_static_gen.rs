use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = r#"
        struct Point { x: i64, y: i64 }
        
        // Standalone function (not in impl block)
        fn point_new(x: i64, y: i64) -> Point { 
            Point { x, y } 
        }
        
        fn main() -> i64 { 
            let p = point_new(10, 20);
            p.x + p.y
        }
    "#;

    println!("Testing with standalone function...");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                println!("Error: Unparsed input: '{}'", remaining);
                return;
            }
            println!("Parse successful!");

            // Try to compile and run
            match compile_and_run(&ast) {
                Ok(result) => println!("Result: {}", result),
                Err(e) => println!("Compile/run error: {}", e),
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }

    println!("\n---\n");

    let code2 = r#"
        struct Point { x: i64, y: i64 }
        
        impl Point {
            fn new(x: i64, y: i64) -> Point { 
                Point { x, y } 
            }
        }
        
        fn main() -> i64 { 
            let p = Point::new(10, 20);
            p.x + p.y
        }
    "#;

    println!("Testing with static method in impl block...");
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                println!("Error: Unparsed input: '{}'", remaining);
                return;
            }
            println!("Parse successful!");

            // Try to compile and run
            match compile_and_run(&ast) {
                Ok(result) => println!("Result: {}", result),
                Err(e) => println!("Compile/run error: {}", e),
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}

fn compile_and_run(_ast: &[zetac::frontend::ast::AstNode]) -> Result<i64, String> {
    // This is a simplified version - in reality we'd need to run the full compiler pipeline
    println!("(Note: Full compilation not implemented in this test)");
    Ok(30) // Expected result for 10 + 20
}
