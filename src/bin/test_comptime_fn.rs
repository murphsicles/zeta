//! Test comptime fn parsing

use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== Testing comptime fn parsing ===\n");
    
    // Test 1: Basic comptime fn
    let test1 = r#"
comptime fn generate_residues() -> [NUM_RESIDUES]u64 {
    // implementation
}
    "#;
    
    println!("Test 1: Basic comptime fn");
    match parse_zeta(test1) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
                for node in &ast {
                    println!("  - {:?}", node);
                }
            } else {
                println!("❌ Partial parse ({} chars remaining: '{}')", remaining.len(), remaining);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
    
    println!("\n---\n");
    
    // Test 2: const fn (should already work)
    let test2 = r#"
const fn add(a: i64, b: i64) -> i64 {
    a + b
}
    "#;
    
    println!("Test 2: const fn (should already work)");
    match parse_zeta(test2) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
                for node in &ast {
                    println!("  - {:?}", node);
                }
            } else {
                println!("❌ Partial parse ({} chars remaining: '{}')", remaining.len(), remaining);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
    
    println!("\n---\n");
    
    // Test 3: pub comptime fn
    let test3 = r#"
pub comptime fn public_comptime_fn() -> i64 {
    42
}
    "#;
    
    println!("Test 3: pub comptime fn");
    match parse_zeta(test3) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
                for node in &ast {
                    println!("  - {:?}", node);
                }
            } else {
                println!("❌ Partial parse ({} chars remaining: '{}')", remaining.len(), remaining);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
    
    println!("\n---\n");
    
    // Test 4: PrimeZeta's comptime fn with array return type
    let test4 = r#"
comptime fn generate_residues() -> [NUM_RESIDUES]u64 {
    let mut residues = [0; NUM_RESIDUES];
    // fill residues
    residues
}
    "#;
    
    println!("Test 4: PrimeZeta's comptime fn with array return type");
    match parse_zeta(test4) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
                for node in &ast {
                    println!("  - {:?}", node);
                }
            } else {
                println!("❌ Partial parse ({} chars remaining: '{}')", remaining.len(), remaining);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}