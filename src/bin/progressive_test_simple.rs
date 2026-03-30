// Progressive Test Suite - Simple Test
// Test 1: identity::<i64>(42)

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::resolver::Resolver;

fn main() {
    println!("=== PROGRESSIVE TEST SUITE ===");
    println!("=== TEST 1: identity::<i64>(42) ===\n");

    let code = r#"
    fn identity<T>(x: T) -> T { x }
    
    fn main() -> i64 {
        identity::<i64>(42)
    }
    "#;

    println!("Code to parse:");
    println!("{}", code);
    println!("{}", "-".repeat(50));

    // Step 1: Parsing
    println!("Step 1: Parsing...");
    match parse_zeta(code) {
        Ok((remaining, asts)) => {
            if !remaining.trim().is_empty() {
                println!("⚠️  Warning: Not all input consumed. Remaining: '{}'", remaining);
            }
            println!("✓ Parse successful");
            println!("  AST count: {}", asts.len());
            
            // Display ASTs
            for (i, ast) in asts.iter().enumerate() {
                println!("  AST {}: {:?}", i, ast);
            }
        }
        Err(e) => {
            println!("✗ Parse failed: {:?}", e);
            return;
        }
    }

    println!("\n{}", "=".repeat(50));
    println!("Test 1 completed.");
}