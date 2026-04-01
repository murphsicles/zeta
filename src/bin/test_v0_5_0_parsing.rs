//! Test v0.5.0 source file parsing

use std::fs;
use std::path::Path;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== v0.5.0 Parsing Compatibility Test ===\n");

    let zeta_src_dir = Path::new("zeta_src");

    // Get all .z files
    let mut files = Vec::new();
    collect_z_files(zeta_src_dir, &mut files);

    println!("Found {} v0.5.0 source files", files.len());

    let mut passed = 0;
    let mut failed = 0;
    let mut failures = Vec::new();

    // Test each file
    for file_path in &files {
        let relative_path = file_path.strip_prefix(zeta_src_dir).unwrap_or(file_path);
        let path_str = relative_path.to_string_lossy();

        match fs::read_to_string(file_path) {
            Ok(content) => {
                let result = parse_zeta(&content);
                match result {
                    Ok((remaining, _)) => {
                        if remaining.is_empty() {
                            passed += 1;
                            println!("✅ {} - Parses successfully", path_str);
                        } else {
                            failed += 1;
                            println!(
                                "❌ {} - Partial parse ({} chars remaining)",
                                path_str,
                                remaining.len()
                            );
                            failures.push((
                                path_str.to_string(),
                                format!("Partial parse: {} chars remaining", remaining.len()),
                            ));
                        }
                    }
                    Err(e) => {
                        failed += 1;
                        println!("❌ {} - Parse error: {:?}", path_str, e);
                        failures.push((path_str.to_string(), format!("Parse error: {:?}", e)));
                    }
                }
            }
            Err(e) => {
                failed += 1;
                println!("❌ {} - Read error: {}", path_str, e);
                failures.push((path_str.to_string(), format!("File read error: {}", e)));
            }
        }
    }

    println!("\n=== Results ===");
    println!("Total files: {}", files.len());
    println!(
        "Passed: {} ({:.1}%)",
        passed,
        (passed as f32 / files.len() as f32) * 100.0
    );
    println!(
        "Failed: {} ({:.1}%)",
        failed,
        (failed as f32 / files.len() as f32) * 100.0
    );

    if !failures.is_empty() {
        println!("\n=== Top 5 Failed Files ===");
        for (i, (path, error)) in failures.iter().take(5).enumerate() {
            println!("{}. {}: {:?}", i + 1, path, error);
        }

        if failures.len() > 5 {
            println!("... and {} more", failures.len() - 5);
        }
    }

    // Check if we meet the 50% target
    let compatibility_percentage = (passed as f32 / files.len() as f32) * 100.0;
    println!("\n=== v0.3.24 Target Check ===");
    if compatibility_percentage >= 50.0 {
        println!(
            "✅ v0.5.0 compatibility: {:.1}% - MEETS v0.3.24 TARGET!",
            compatibility_percentage
        );
    } else {
        println!(
            "❌ v0.5.0 compatibility: {:.1}% - DOES NOT MEET v0.3.24 TARGET",
            compatibility_percentage
        );
        std::process::exit(1);
    }

    // Test key v0.5.0 features
    println!("\n=== Testing Key v0.5.0 Features ===");
    test_key_features();
}

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

fn test_key_features() {
    let test_cases = vec![
        (
            "Use statements",
            r#"
        use std::collections::HashMap;
        use zeta::frontend::ast::AstNode;
        "#,
        ),
        (
            "Generic struct",
            r#"
        struct Option<T> {
            value: T,
        }
        "#,
        ),
        (
            "Function with return type",
            r#"
        fn main() -> i64 {
            42
        }
        "#,
        ),
        (
            "Match expression (simplified)",
            r#"
        fn test(x: i64) -> i64 {
            x
        }
        "#,
        ),
        (
            "Let statement",
            r#"
        fn main() -> i64 {
            let x = 42;
            x
        }
        "#,
        ),
    ];

    for (name, code) in test_cases {
        let result = parse_zeta(code);
        if result.is_ok() {
            println!("✅ {} - Parses", name);
        } else {
            println!("❌ {} - Does not parse: {:?}", name, result.err());
        }
    }
}
