//! Test comptime array generation

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::ConstEvaluator;

fn main() {
    println!("=== Testing comptime array generation ===\n");

    // Test 1: Simple array literal in comptime function
    let test1 = r#"
comptime fn simple_array() -> [i64; 3] {
    [1, 2, 3]
}
    "#;

    println!("Test 1: Simple array literal in comptime function");
    match parse_zeta(test1) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
                
                // Try to evaluate (simplified - just check parsing for now)
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(Some(value)) => {
                            println!("⚠️  Evaluates to scalar: {}", value);
                        }
                        Ok(None) => {
                            println!("❌ Does not evaluate (returns None) - expected for arrays");
                        }
                        Err(e) => {
                            println!("❌ Evaluation error: {}", e);
                        }
                    }
                }
            } else {
                println!("❌ Partial parse");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }

    println!("\n---\n");

    // Test 2: Array with comptime loop (simplified)
    let test2 = r#"
const SIZE: usize = 5
comptime fn generate_sequence() -> [i64; SIZE] {
    // This would need loop evaluation
    [0; SIZE]
}
    "#;

    println!("Test 2: Array with comptime loop (simplified)");
    match parse_zeta(test2) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
            } else {
                println!("❌ Partial parse");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }

    println!("\n---\n");

    // Test 3: PrimeZeta-style residue generation (simplified)
    let test3 = r#"
const MODULUS: u64 = 10
const NUM_RESIDUES: usize = 4

comptime fn generate_residues() -> [NUM_RESIDUES]u64 {
    // Simplified version - actual PrimeZeta has gcd check
    var list: [NUM_RESIDUES]u64 = [0; NUM_RESIDUES]
    list[0] = 1
    list[1] = 3
    list[2] = 7
    list[3] = 9
    return list
}
    "#;

    println!("Test 3: PrimeZeta-style residue generation (simplified)");
    match parse_zeta(test3) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("  AST has {} nodes", ast.len());
            } else {
                println!("❌ Partial parse");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }

    println!("\n=== All tests completed ===");
    println!("\n=== NOTE ===");
    println!("Array evaluation in comptime functions is not fully implemented yet.");
    println!("Current implementation only supports simple integer return values.");
}