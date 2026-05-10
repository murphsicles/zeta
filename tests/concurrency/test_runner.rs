// tests/concurrency/test_runner.rs
// Test runner for concurrency features

use std::fs;
use std::path::Path;
use zetac::middle::resolver::new_resolver;

fn test_file(path: &Path) -> Result<(), String> {
    let code = fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;
    
    println!("Testing {}...", path.display());
    
    // Try to parse the file
    let (remaining, asts) = zetac::parse_zeta(&code)
        .map_err(|e| format!("Parse error: {:?}", e))?;
    
    // Check that all input was consumed
    let trimmed_remaining = remaining.trim();
    if !trimmed_remaining.is_empty() {
        return Err(format!("Incomplete parse. Remaining: '{}'", trimmed_remaining));
    }
    
    println!("  ✓ Parsed successfully ({} AST nodes)", asts.len());
    
    // Try to resolve types
    let mut resolver = new_resolver::InferContext::new();
    for ast in &asts {
        resolver.infer(ast)
            .map_err(|e| format!("Type error: {}", e))?;
    }
    
    println!("  ✓ Type checking passed");
    
    Ok(())
}

fn main() -> Result<(), String> {
    let test_dir = Path::new("tests/concurrency");
    
    if !test_dir.exists() {
        return Err("Test directory not found".to_string());
    }
    
    println!("Running concurrency tests...");
    
    // Test async/await syntax
    test_file(&test_dir.join("async_await_syntax.z"))?;
    
    // Test future trait
    test_file(&test_dir.join("future_concept.z"))?;
    
    // Test basic executor
    test_file(&test_dir.join("basic_executor.z"))?;
    
    // Test async patterns
    test_file(&test_dir.join("async_patterns.z"))?;
    
    // Test simple async
    test_file(&test_dir.join("simple_async_test.z"))?;
    
    println!("\nAll concurrency tests passed!");
    Ok(())
}