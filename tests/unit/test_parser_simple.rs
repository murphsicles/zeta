use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Simple test without module syntax first
    let code = r#"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        
        fn main() -> i64 {
            add(10, 20)
        }
    "#;
    
    println!("Testing basic function parsing...");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("Number of AST nodes: {}", ast.len());
            for (i, node) in ast.iter().enumerate() {
                println!("Node {}: {:?}", i, node);
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Now test with module syntax
    let code2 = r#"
        mod math {
            pub fn add(a: i64, b: i64) -> i64 {
                a + b
            }
        }
        
        fn main() -> i64 {
            42
        }
    "#;
    
    println!("\nTesting module parsing...");
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("Number of AST nodes: {}", ast.len());
            for (i, node) in ast.iter().enumerate() {
                println!("Node {}: {:?}", i, node);
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}