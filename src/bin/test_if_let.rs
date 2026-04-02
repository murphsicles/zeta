//! Test if-let parsing

use zetac::frontend::parser::stmt::parse_stmt;

fn main() {
    println!("Testing if-let parsing directly");
    
    // Test 1: Simple if-let
    let code = "if let x = y { println!(\"x\"); }";
    println!("Test 1: {}", code);
    match parse_stmt(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Test 2: if-let with else
    let code = "if let x = y { 1 } else { 2 }";
    println!("\nTest 2: {}", code);
    match parse_stmt(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Test 3: if-let with pattern
    let code = "if let Ok(x) = result { x } else { 0 }";
    println!("\nTest 3: {}", code);
    match parse_stmt(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}