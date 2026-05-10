use std::fs;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test 1: Simple function
    let code1 = r#"fn simple() -> i64 { return 42; }"#;
    
    println!("Test 1: Simple function");
    match parse_zeta(code1) {
        Ok((remaining, ast)) => {
            println!("  Success! AST has {} nodes", ast.len());
            if !remaining.trim().is_empty() {
                println!("  Remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("  Error: {:?}", e);
        }
    }
    
    // Test 2: Const declaration
    let code2 = r#"const N: usize = 100;"#;
    
    println!("\nTest 2: Const declaration");
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            println!("  Success! AST has {} nodes", ast.len());
            if !remaining.trim().is_empty() {
                println!("  Remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("  Error: {:?}", e);
        }
    }
    
    // Test 3: Comptime function
    let code3 = r#"comptime fn test() -> i64 { return 42; }"#;
    
    println!("\nTest 3: Comptime function");
    match parse_zeta(code3) {
        Ok((remaining, ast)) => {
            println!("  Success! AST has {} nodes", ast.len());
            if !remaining.trim().is_empty() {
                println!("  Remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("  Error: {:?}", e);
        }
    }
}