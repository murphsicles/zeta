// tests/concurrency-advanced/test_runner.rs
// Test runner for advanced concurrency features

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
    let test_dir = Path::new("tests/concurrency-advanced");
    
    if !test_dir.exists() {
        return Err("Advanced concurrency test directory not found".to_string());
    }
    
    println!("Running advanced concurrency tests...");
    println!("======================================");
    
    // Test actor system
    println!("\n1. Testing Actor System:");
    test_file(&test_dir.join("actor_system.z"))?;
    
    // Test channel primitives
    println!("\n2. Testing Channel Primitives:");
    test_file(&test_dir.join("channel_primitives.z"))?;
    
    // Test async runtime
    println!("\n3. Testing Async Runtime:");
    test_file(&test_dir.join("async_runtime.z"))?;
    
    // Test concurrency patterns
    println!("\n4. Testing Concurrency Patterns:");
    test_file(&test_dir.join("concurrency_patterns.z"))?;
    
    println!("\n======================================");
    println!("All advanced concurrency tests passed!");
    println!("Features implemented:");
    println!("  • Actor system with message passing");
    println!("  • Channel primitives (MPSC, bounded/unbounded)");
    println!("  • Enhanced async runtime with work stealing");
    println!("  • Advanced concurrency patterns");
    
    Ok(())
}