//! Test PrimeZeta const and comptime parsing

use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== PrimeZeta Const/Comptime Parsing Test ===\n");

    // Test 1: Basic const declarations
    let test1 = r#"
const MODULUS: u64 = 30030
const NUM_RESIDUES: usize = 5760
    "#;

    println!("Test 1: Basic const declarations");
    match parse_zeta(test1) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
                for node in &ast {
                    println!("  - {:?}", node);
                }
            } else {
                println!(
                    "❌ Partial parse ({} chars remaining: '{}')",
                    remaining.len(),
                    remaining
                );
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }

    println!("\n---\n");

    // Test 2: Array type in const (no semicolon)
    let test2 = r#"
const ARR: [i64; 3] = [1, 2, 3]
    "#;

    println!("Test 2: Array type in const (no semicolon)");
    match parse_zeta(test2) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
            } else {
                println!(
                    "❌ Partial parse ({} chars remaining: '{}')",
                    remaining.len(),
                    remaining
                );
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }

    println!("\n---\n");

    // Test 3: Array type in const (with semicolon - backward compatibility)
    let test3 = r#"
const ARR2: [i64; 3] = [1, 2, 3];
    "#;

    println!("Test 3: Array type in const (with semicolon)");
    match parse_zeta(test3) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
            } else {
                println!(
                    "❌ Partial parse ({} chars remaining: '{}')",
                    remaining.len(),
                    remaining
                );
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }

    println!("\n---\n");

    // Test 4: Array type with const expression size
    let test4 = r#"
const SIZE: usize = 10
const ARR3: [i64; SIZE] = [0; SIZE]
    "#;

    println!("Test 4: Array type with const expression size");
    match parse_zeta(test4) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
                for node in &ast {
                    println!("  - {:?}", node);
                }
            } else {
                println!(
                    "❌ Partial parse ({} chars remaining: '{}')",
                    remaining.len(),
                    remaining
                );
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }

    println!("\n---\n");

    // Test 5: Comptime variables
    let test5 = r#"
comptime MODULUS: u64 = 30030
comptime NUM_RESIDUES: usize = 5760
    "#;

    println!("Test 5: Comptime variables");
    match parse_zeta(test5) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
                for node in &ast {
                    println!("  - {:?}", node);
                }
            } else {
                println!(
                    "❌ Partial parse ({} chars remaining: '{}')",
                    remaining.len(),
                    remaining
                );
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}
