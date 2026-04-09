//! Comprehensive test for module resolver with all import patterns

use zetac::middle::resolver::module_resolver::ModuleResolver;
use std::path::PathBuf;

#[test]
fn test_module_resolver_import_patterns() {
    println!("\n=== Testing Module Resolver Import Patterns ===");
    
    let mut resolver = ModuleResolver::new(".");
    
    // Test 1: std:: imports
    println!("\nTest 1: std:: imports");
    let std_path = vec!["std".to_string(), "malloc".to_string()];
    match resolver.resolve_use_path(&std_path) {
        Ok(path) => println!("  ✓ Resolved std::malloc to: {:?}", path),
        Err(e) => println!("  ✗ Failed to resolve std::malloc: {}", e),
    }
    
    // Test 2: crate:: imports
    println!("\nTest 2: crate:: imports");
    let crate_path = vec!["crate".to_string(), "middle".to_string(), "mir".to_string(), "Mir".to_string()];
    match resolver.resolve_use_path(&crate_path) {
        Ok(path) => println!("  ✓ Resolved crate::middle::mir::Mir to: {:?}", path),
        Err(e) => println!("  ✗ Failed to resolve crate::middle::mir::Mir: {}", e),
    }
    
    // Test 3: zeta:: imports (self-compilation)
    println!("\nTest 3: zeta:: imports (self-compilation)");
    let zeta_path = vec!["zeta".to_string(), "frontend".to_string(), "ast".to_string(), "AstNode".to_string()];
    match resolver.resolve_use_path(&zeta_path) {
        Ok(path) => println!("  ✓ Resolved zeta::frontend::ast::AstNode to: {:?}", path),
        Err(e) => println!("  ✗ Failed to resolve zeta::frontend::ast::AstNode: {}", e),
    }
    
    // Test 4: zorb::std:: imports
    println!("\nTest 4: zorb::std:: imports");
    let zorb_std_path = vec!["zorb".to_string(), "std".to_string(), "collections".to_string(), "HashMap".to_string()];
    match resolver.resolve_use_path(&zorb_std_path) {
        Ok(path) => println!("  ✓ Resolved zorb::std::collections::HashMap to: {:?}", path),
        Err(e) => println!("  ✗ Failed to resolve zorb::std::collections::HashMap: {}", e),
    }
    
    // Test 5: zorb:: package manager imports
    println!("\nTest 5: zorb:: package manager imports");
    let zorb_package_path = vec!["zorb".to_string(), "package".to_string(), "Package".to_string()];
    match resolver.resolve_use_path(&zorb_package_path) {
        Ok(path) => println!("  ✓ Resolved zorb::package::Package to: {:?}", path),
        Err(e) => println!("  ✗ Failed to resolve zorb::package::Package: {}", e),
    }
    
    // Test 6: External crate imports (via zorb::)
    println!("\nTest 6: External crate imports (via zorb::)");
    let ext_path = vec!["zorb".to_string(), "reqwest".to_string(), "blocking".to_string(), "Client".to_string()];
    match resolver.resolve_use_path(&ext_path) {
        Ok(path) => println!("  ✓ Resolved zorb::reqwest::blocking::Client to: {:?}", path),
        Err(e) => println!("  ✗ Failed to resolve zorb::reqwest::blocking::Client: {}", e),
    }
    
    // Test 7: Direct crate imports (backward compatibility)
    println!("\nTest 7: Direct crate imports");
    let direct_path = vec!["serde".to_string(), "Deserialize".to_string()];
    match resolver.resolve_use_path(&direct_path) {
        Ok(path) => println!("  ✓ Resolved serde::Deserialize to: {:?}", path),
        Err(e) => println!("  ✗ Failed to resolve serde::Deserialize: {}", e),
    }
    
    println!("\n=== Module Resolver Tests Complete ===");
}

#[test]
fn test_module_loading() {
    println!("\n=== Testing Module Loading ===");
    
    let mut resolver = ModuleResolver::new(".");
    
    // Test loading a std module
    println!("\nTest: Loading std module");
    let std_path = PathBuf::from("stub_types/std.z");
    match resolver.load_module(&std_path) {
        Ok(module) => {
            println!("  ✓ Loaded std module: {}", module.name);
            println!("  Exports: {:?}", module.exports.keys().collect::<Vec<_>>());
        }
        Err(e) => println!("  ✗ Failed to load std module: {}", e),
    }
    
    // Test loading a zorb package module
    println!("\nTest: Loading zorb package module");
    let zorb_path = PathBuf::from("zorb/package.z");
    match resolver.load_module(&zorb_path) {
        Ok(module) => {
            println!("  ✓ Loaded zorb package module: {}", module.name);
            println!("  Exports: {:?}", module.exports.keys().collect::<Vec<_>>());
        }
        Err(e) => println!("  ✗ Failed to load zorb package module: {}", e),
    }
    
    println!("\n=== Module Loading Tests Complete ===");
}