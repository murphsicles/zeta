//! Quick check of v0.5.0 compatibility percentage

use std::fs;
use std::path::Path;
use zetac::frontend::parser::top_level::parse_zeta;

#[test]
fn check_v0_5_0_compatibility_percentage() {
    let zeta_src_dir = Path::new("zeta_src");
    
    // Count total .z files
    let total_files = count_z_files(zeta_src_dir);
    println!("Total v0.5.0 source files: {}", total_files);
    
    // Sample a few files to check parsing
    let sample_files = vec![
        "zeta_src/main.z",
        "zeta_src/frontend/ast.z",
        "zeta_src/frontend/parser/top_level.z",
        "zeta_src/middle/resolver/resolver.z",
        "zeta_src/backend/codegen/codegen.z",
    ];
    
    let mut passed = 0;
    
    for file_path in sample_files {
        let path = Path::new(file_path);
        if path.exists() {
            match fs::read_to_string(path) {
                Ok(content) => {
                    let result = parse_zeta(&content);
                    if result.is_ok() {
                        passed += 1;
                        println!("✅ {} - Parses", path.display());
                    } else {
                        println!("❌ {} - Parse error", path.display());
                    }
                }
                Err(e) => {
                    println!("❌ {} - Read error: {}", path.display(), e);
                }
            }
        } else {
            println!("⚠️  {} - Not found", file_path);
        }
    }
    
    // Estimate compatibility based on sample
    let sample_size = sample_files.len();
    let estimated_compatibility = (passed as f32 / sample_size as f32) * 100.0;
    
    println!("\n=== v0.5.0 Compatibility Estimate ===");
    println!("Sample size: {} files", sample_size);
    println!("Parsed successfully: {} files", passed);
    println!("Estimated compatibility: {:.1}%", estimated_compatibility);
    
    // Check if we meet the 50% target for v0.3.24
    assert!(
        estimated_compatibility >= 50.0,
        "Estimated v0.5.0 compatibility is {:.1}%, need at least 50% for v0.3.24",
        estimated_compatibility
    );
    
    println!("✅ Meets v0.3.24 target of 50%+ compatibility!");
}

fn count_z_files(dir: &Path) -> usize {
    let mut count = 0;
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                count += count_z_files(&path);
            } else if path.extension().and_then(|s| s.to_str()) == Some("z") {
                count += 1;
            }
        }
    }
    count
}

#[test]
fn test_v0_5_0_key_features() {
    println!("\n=== Testing v0.5.0 Key Features ===");
    
    // Test 1: Use statements (common in v0.5.0)
    let code = r#"
    use std::collections::HashMap;
    use zeta::frontend::ast::AstNode;
    "#;
    
    let result = parse_zeta(code);
    if result.is_ok() {
        println!("✅ Use statements parse");
    } else {
        println!("❌ Use statements don't parse: {:?}", result.err());
    }
    
    // Test 2: Generic structs
    let code = r#"
    struct Option<T> {
        value: T,
    }
    "#;
    
    let result = parse_zeta(code);
    if result.is_ok() {
        println!("✅ Generic structs parse");
    } else {
        println!("❌ Generic structs don't parse: {:?}", result.err());
    }
    
    // Test 3: Trait bounds
    let code = r#"
    fn clone_and_print<T: Clone + Display>(item: T) {
        let cloned = item.clone();
        println!("{}", cloned);
    }
    "#;
    
    let result = parse_zeta(code);
    if result.is_ok() {
        println!("✅ Trait bounds parse");
    } else {
        println!("❌ Trait bounds don't parse: {:?}", result.err());
    }
    
    // Test 4: Match expressions
    let code = r#"
    fn handle_option(opt: Option<i64>) -> i64 {
        match opt {
            Some(value) => value,
            None => 0,
        }
    }
    "#;
    
    let result = parse_zeta(code);
    if result.is_ok() {
        println!("✅ Match expressions parse");
    } else {
        println!("❌ Match expressions don't parse: {:?}", result.err());
    }
    
    // Test 5: Impl blocks
    let code = r#"
    impl<T> Option<T> {
        fn unwrap(self) -> T {
            self.value
        }
    }
    "#;
    
    let result = parse_zeta(code);
    if result.is_ok() {
        println!("✅ Impl blocks parse");
    } else {
        println!("❌ Impl blocks don't parse: {:?}", result.err());
    }
}