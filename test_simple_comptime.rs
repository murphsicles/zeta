// Simple test for comptime evaluation
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("Testing comptime parsing...");
    
    // Test 1: Simple comptime function
    let code = r#"
comptime fn add() -> i64 {
    return 2 + 3;
}
    "#;
    
    match parse_zeta(code) {
        Ok((remaining, asts)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parsed successfully!");
                println!("Number of AST nodes: {}", asts.len());
                for (i, ast) in asts.iter().enumerate() {
                    println!("AST {}: {:?}", i, ast);
                }
            } else {
                println!("❌ Partial parse. Remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}