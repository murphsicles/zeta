use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test 1: Simple use statement
    let code = "use std::collections::HashMap;";
    println!("Test 1: Simple use statement");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully!");
            println!("Remaining: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            for node in ast {
                println!("  Node: {:?}", node);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
    
    println!("\n---\n");
    
    // Test 2: Module declaration
    let code = r#"
    mod my_module {
        pub fn hello() -> i32 {
            42
        }
    }
    "#;
    println!("Test 2: Module declaration");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully!");
            println!("Remaining: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            for node in ast {
                println!("  Node: {:?}", node);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
    
    println!("\n---\n");
    
    // Test 3: Renamed import (should fail with current parser)
    let code = "use std::io as stdio;";
    println!("Test 3: Renamed import (as keyword)");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully!");
            println!("Remaining: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            for node in ast {
                println!("  Node: {:?}", node);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
    
    println!("\n---\n");
    
    // Test 4: Glob import (should fail with current parser)
    let code = "use std::prelude::*;";
    println!("Test 4: Glob import (*)");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully!");
            println!("Remaining: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            for node in ast {
                println!("  Node: {:?}", node);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}