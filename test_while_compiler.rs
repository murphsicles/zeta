use std::fs;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test 1: Simple while loop
    let test1 = "while i < 10 { i = i + 1 }";
    println!("Test 1: {}", test1);
    match parse_zeta(test1) {
        Ok((remaining, asts)) => {
            println!("  ✓ Parsed successfully!");
            println!("  ASTs: {:?}", asts);
            if !remaining.is_empty() {
                println!("  Warning: Remaining input: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("  ✗ Parse error: {:?}", e);
        }
    }
    
    println!("\n---\n");
    
    // Test 2: While loop with more complex condition
    let test2 = "while x > 0 && x < 100 { x = x - 1 }";
    println!("Test 2: {}", test2);
    match parse_zeta(test2) {
        Ok((remaining, asts)) => {
            println!("  ✓ Parsed successfully!");
            if !remaining.is_empty() {
                println!("  Warning: Remaining input: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("  ✗ Parse error: {:?}", e);
        }
    }
    
    println!("\n---\n");
    
    // Test 3: Nested while loop
    let test3 = "while i < 10 { while j < 5 { j = j + 1 } i = i + 1 }";
    println!("Test 3: {}", test3);
    match parse_zeta(test3) {
        Ok((remaining, asts)) => {
            println!("  ✓ Parsed successfully!");
            if !remaining.is_empty() {
                println!("  Warning: Remaining input: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("  ✗ Parse error: {:?}", e);
        }
    }
}