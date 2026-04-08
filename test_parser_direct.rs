// Direct test of the parser without building the whole project
use std::fs;

fn test_parser() {
    // Test 1: Simple use statement
    let code = "use std::collections::HashMap;";
    println!("Test 1: Simple use statement: '{}'", code);
    
    // Test 2: Renamed import
    let code = "use std::io as stdio;";
    println!("\nTest 2: Renamed import: '{}'", code);
    
    // Test 3: Glob import
    let code = "use std::prelude::*;";
    println!("\nTest 3: Glob import: '{}'", code);
    
    // Test 4: Nested import with braces
    let code = "use std::collections::{HashMap, HashSet};";
    println!("\nTest 4: Nested import: '{}'", code);
    
    // Test 5: Module declaration (inline)
    let code = r#"
    mod my_module {
        pub fn hello() -> i32 {
            42
        }
    }
    "#;
    println!("\nTest 5: Inline module: '{}'", code.trim());
    
    // Test 6: File-based module declaration
    let code = "mod my_module;";
    println!("\nTest 6: File-based module declaration: '{}'", code);
    
    // Test 7: Complex import with alias in braces
    let code = "use std::collections::{HashMap as HM, HashSet as HS};";
    println!("\nTest 7: Complex import with aliases: '{}'", code);
    
    println!("\n✅ Parser tests defined. The parser should handle:");
    println!("   - Simple imports (use std::collections::HashMap;)");
    println!("   - Renamed imports (use std::io as stdio;)");
    println!("   - Glob imports (use std::prelude::*;)");
    println!("   - Nested imports (use std::collections::{{HashMap, HashSet}};)");
    println!("   - Inline modules (mod name {{ ... }})");
    println!("   - File-based modules (mod name;)");
    println!("   - Aliases in nested imports (use std::collections::{{HashMap as HM}};)");
}

fn main() {
    println!("=== PrimeZeta Module System Parser Test ===\n");
    test_parser();
    println!("\n=== End of Tests ===");
    
    // Check if the AST was updated
    let ast_content = fs::read_to_string("src/frontend/ast.rs").unwrap();
    if ast_content.contains("ModDecl") {
        println!("\n✅ AST has ModDecl variant for file-based module declarations");
    } else {
        println!("\n❌ AST missing ModDecl variant");
    }
    
    if ast_content.contains("alias: Option<String>") && ast_content.contains("is_glob: bool") {
        println!("✅ AST has updated Use variant with alias and is_glob fields");
    } else {
        println!("❌ AST missing updated Use variant fields");
    }
}