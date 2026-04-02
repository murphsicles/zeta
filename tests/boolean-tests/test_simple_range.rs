use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test exclusive range
    let code1 = r#"let r = 1..10;"#;
    println!("Testing: {}", code1);
    match parse_zeta(code1) {
        Ok((remaining, ast)) => {
            println!("Success! AST: {:?}", ast);
            // Check if it's a Range node
            if let Some(node) = ast.first() {
                println!("First node type: {:?}", std::any::type_name_of_val(node));
            }
        }
        Err(e) => println!("Error: {:?}", e),
    }
    
    // Test inclusive range
    let code2 = r#"let r = 1..=10;"#;
    println!("\nTesting: {}", code2);
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            println!("Success! AST: {:?}", ast);
        }
        Err(e) => println!("Error: {:?}", e),
    }
    
    // Test for loop with range
    let code3 = r#"for i in 1..10 { println!("{}", i); }"#;
    println!("\nTesting: {}", code3);
    match parse_zeta(code3) {
        Ok((remaining, ast)) => {
            println!("Success! AST has {} nodes", ast.len());
        }
        Err(e) => println!("Error: {:?}", e),
    }
}