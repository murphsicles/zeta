//! Test simple parsing

use zetac::frontend::parser::top_level::parse_zeta;
use std::fs;

fn test_parse(filename: &str) -> Result<(), String> {
    println!("\n=== Testing {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .map_err(|e| format!("Failed to read file {}: {}", filename, e))?;
    
    match parse_zeta(&content) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                println!("AST has {} nodes", ast.len());
                for (i, node) in ast.iter().enumerate() {
                    println!("  Node {}: {:?}", i, node);
                }
                Ok(())
            } else {
                Err(format!("Partial parse, remaining: {}", remaining))
            }
        }
        Err(e) => {
            Err(format!("Parse error: {:?}", e))
        }
    }
}

fn main() {
    println!("=== Simple Parse Tests ===\n");
    
    let test_files = [
        "tests/traits-advanced/test_basic_concepts.z",
        "tests/traits-advanced/test_default_methods.z",
        "tests/traits-advanced/test_comprehensive_simple.z",
        "tests/comptime-completion/test_simple_for.z",
        "tests/comptime-completion/test_variable_assignment.z",
    ];
    
    for file in &test_files {
        match test_parse(file) {
            Ok(_) => println!("✅ PASS: {}", file),
            Err(e) => println!("❌ FAIL: {} - {}", file, e),
        }
    }
}