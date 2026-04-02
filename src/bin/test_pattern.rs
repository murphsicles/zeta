//! Test pattern parsing

use zetac::frontend::parser::pattern::parse_pattern;

fn main() {
    println!("Testing pattern parsing");
    
    // Test 1: Variable pattern
    let code = "x";
    println!("Test 1: {}", code);
    match parse_pattern(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Test 2: Ignore pattern
    let code = "_";
    println!("\nTest 2: {}", code);
    match parse_pattern(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Test 3: Tuple struct pattern
    let code = "Ok(x)";
    println!("\nTest 3: {}", code);
    match parse_pattern(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Test 4: Literal pattern
    let code = "42";
    println!("\nTest 4: {}", code);
    match parse_pattern(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}