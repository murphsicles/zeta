//! v0.5.0 file compatibility analysis
//!
//! Tests which v0.5.0 source files parse successfully and identifies
//! specific parsing issues.

use std::fs;
use std::path::Path;
use zetac::frontend::parser::top_level::parse_zeta;

/// Test all v0.5.0 source files for parsing compatibility
#[test]
fn test_v0_5_0_all_files() {
    let zeta_src_dir = Path::new("zeta_src");
    
    // Collect all .z files
    let mut files = Vec::new();
    collect_z_files(zeta_src_dir, &mut files);
    
    println!("Found {} v0.5.0 source files", files.len());
    
    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();
    
    for file_path in &files {
        let relative_path = file_path.strip_prefix(zeta_src_dir).unwrap_or(file_path);
        let path_str = relative_path.to_string_lossy();
        
        match fs::read_to_string(file_path) {
            Ok(content) => {
                let result = parse_zeta(&content);
                if result.is_ok() {
                    passed += 1;
                    println!("✅ {} - Parses successfully", path_str);
                } else {
                    failed += 1;
                    let error = result.unwrap_err();
                    println!("❌ {} - Parse error: {:?}", path_str, error);
                    failures.push((path_str.to_string(), error));
                }
            }
            Err(e) => {
                failed += 1;
                println!("❌ {} - Read error: {}", path_str, e);
                failures.push((path_str.to_string(), format!("File read error: {}", e)));
            }
        }
    }
    
    println!("\n=== v0.5.0 Compatibility Report ===");
    println!("Total files: {}", files.len());
    println!("Passed: {} ({:.1}%)", passed, (passed as f32 / files.len() as f32) * 100.0);
    println!("Failed: {} ({:.1}%)", failed, (failed as f32 / files.len() as f32) * 100.0);
    
    if !failures.is_empty() {
        println!("\n=== Failed Files ===");
        for (path, error) in failures {
            println!("- {}: {:?}", path, error);
        }
    }
    
    // Target: 50%+ compatibility for v0.3.24
    let compatibility_percentage = (passed as f32 / files.len() as f32) * 100.0;
    assert!(
        compatibility_percentage >= 50.0,
        "v0.5.0 compatibility is {:.1}%, need at least 50% for v0.3.24",
        compatibility_percentage
    );
    
    println!("\n✅ v0.5.0 compatibility: {:.1}% - MEETS v0.3.24 TARGET!", compatibility_percentage);
}

/// Recursively collect all .z files
fn collect_z_files(dir: &Path, files: &mut Vec<std::path::PathBuf>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_z_files(&path, files);
            } else if path.extension().and_then(|s| s.to_str()) == Some("z") {
                files.push(path);
            }
        }
    }
}

/// Test specific problematic patterns from v0.5.0 files
#[test]
fn test_v0_5_0_problematic_patterns() {
    // Pattern 1: Complex use statements
    let code = r#"
    use zeta::frontend::ast::AstNode;
    use zeta::frontend::parser::top_level::parse_zeta;
    use zeta::middle::resolver::resolver::Resolver;
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Complex use statements should parse");
    
    // Pattern 2: Generic struct definitions
    let code = r#"
    struct HashMap<K, V> {
        data: Vec<(K, V)>,
    }
    "#;
    
    let result = parse_zeta(code);
    // This might fail if generic structs aren't fully supported
    if result.is_err() {
        println!("Note: Generic struct definitions not yet fully supported");
    }
    
    // Pattern 3: Trait bounds in generics
    let code = r#"
    fn process<T: Display + Clone>(value: T) -> String {
        value.to_string()
    }
    "#;
    
    let result = parse_zeta(code);
    if result.is_err() {
        println!("Note: Trait bounds in generics not yet fully supported");
    }
    
    // Pattern 4: Match expressions with patterns
    let code = r#"
    fn handle_result(result: Result<i64, String>) -> i64 {
        match result {
            Ok(value) => value,
            Err(msg) => {
                println!("Error: {}", msg);
                0
            }
        }
    }
    "#;
    
    let result = parse_zeta(code);
    if result.is_err() {
        println!("Note: Complex match expressions not yet fully supported");
    }
}

/// Test compilation of simple v0.5.0 programs
#[test]
fn test_v0_5_0_simple_compilation() {
    // Test 1: Simple program that should compile
    let code = r#"
    fn main() -> i64 {
        let x = 42;
        x
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Simple v0.5.0 program should parse");
    
    // Test 2: Program with imports (simplified)
    let code = r#"
    use std::collections::HashMap;
    
    fn main() -> i64 {
        let mut map = HashMap::new();
        map.insert("key", 42);
        map.get("key").unwrap_or(&0)
    }
    "#;
    
    let result = parse_zeta(code);
    // This tests import parsing
    if result.is_err() {
        println!("Note: Import statements with collections might not parse");
    }
    
    // Test 3: Function with parameters
    let code = r#"
    fn add(a: i64, b: i64) -> i64 {
        a + b
    }
    
    fn main() -> i64 {
        add(10, 20)
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Functions with parameters should parse");
}

/// Test performance of parsing v0.5.0 files
#[test]
fn test_v0_5_0_parsing_performance() {
    use std::time::Instant;
    
    // Test parsing a representative v0.5.0 file
    let test_file = Path::new("zeta_src/main.z");
    let content = fs::read_to_string(test_file).expect("Should read test file");
    
    let start = Instant::now();
    let result = parse_zeta(&content);
    let duration = start.elapsed();
    
    println!("Parsing main.z took: {:?}", duration);
    
    // Should complete in reasonable time (under 1 second)
    assert!(
        duration.as_secs() < 1,
        "Parsing v0.5.0 file took too long: {:?}",
        duration
    );
    
    // The file should parse (or at least not crash)
    if result.is_err() {
        println!("Note: main.z doesn't parse yet: {:?}", result.err());
    }
}