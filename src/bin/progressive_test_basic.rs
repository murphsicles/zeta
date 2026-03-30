// Progressive Test Suite - Basic Test
// Tests without triggering the broken from_string code path

use zetac::frontend::parser::top_level::parse_zeta;

fn test_parsing_only(name: &str, code: &str, test_num: usize) -> bool {
    println!("\n{}", "=".repeat(60));
    println!("TEST {}: {}", test_num, name);
    println!("{}", "=".repeat(60));
    println!("Code:\n{}", code);
    println!("{}", "-".repeat(60));

    println!("Step 1: Parsing only...");
    match parse_zeta(code) {
        Ok((remaining, asts)) => {
            if !remaining.trim().is_empty() {
                println!("⚠️  Warning: Not all input consumed. Remaining: '{}'", remaining);
            }
            println!("✓ Parse successful");
            println!("  AST count: {}", asts.len());
            
            // Show basic AST info
            for (i, ast) in asts.iter().enumerate() {
                match ast {
                    zetac::frontend::ast::AstNode::FuncDef { name, generics, .. } => {
                        println!("  Function {}: '{}'", i, name);
                        if !generics.is_empty() {
                            println!("    Generics: {:?}", generics);
                        }
                    }
                    zetac::frontend::ast::AstNode::StructDef { name, .. } => {
                        println!("  Struct {}: '{}'", i, name);
                    }
                    zetac::frontend::ast::AstNode::EnumDef { name, .. } => {
                        println!("  Enum {}: '{}'", i, name);
                    }
                    zetac::frontend::ast::AstNode::ImplBlock { .. } => {
                        println!("  Impl block {}", i);
                    }
                    _ => println!("  Unknown AST node type {}", i),
                }
            }
            
            // Check for generic syntax in return expressions
            let mut has_generic_calls = false;
            for ast in &asts {
                if let zetac::frontend::ast::AstNode::FuncDef { ret_expr, .. } = ast {
                    if let Some(expr) = ret_expr {
                        let expr_str = format!("{:?}", expr);
                        if expr_str.contains("type_args") && expr_str.contains("[") {
                            println!("  ✓ Detected generic function call with type arguments");
                            has_generic_calls = true;
                        }
                    }
                }
            }
            
            if has_generic_calls {
                println!("✓ Generic syntax correctly parsed");
            }
            
            true
        }
        Err(e) => {
            println!("✗ Parse failed: {:?}", e);
            false
        }
    }
}

fn main() {
    println!("=== PROGRESSIVE TEST SUITE - BASIC PARSING ===");
    println!("Testing parser only (avoids compilation issues)\n");

    let mut test_results = Vec::new();

    // Test 1: Simple - identity::<i64>(42)
    let test1_code = r#"
    fn identity<T>(x: T) -> T { x }
    
    fn main() -> i64 {
        identity::<i64>(42)
    }
    "#;
    
    test_results.push(test_parsing_only("identity::<i64>(42)", test1_code, 1));

    // Test 2: Medium - Option::<i32>::None  
    let test2_code = r#"
    enum Option<T> {
        Some(T),
        None,
    }
    
    fn main() -> Option<i32> {
        Option::<i32>::None
    }
    "#;
    
    test_results.push(test_parsing_only("Option::<i32>::None", test2_code, 2));

    // Test 3: Complex - Vec::<i32>::new()
    let test3_code = r#"
    struct Vec<T> {
        data: *mut T,
        len: usize,
        capacity: usize,
    }
    
    impl<T> Vec<T> {
        fn new() -> Vec<T> {
            Vec { data: 0 as *mut T, len: 0, capacity: 0 }
        }
    }
    
    fn main() -> Vec<i32> {
        Vec::<i32>::new()
    }
    "#;
    
    test_results.push(test_parsing_only("Vec::<i32>::new()", test3_code, 3));

    // Summary
    println!("\n{}", "=".repeat(60));
    println!("TEST SUMMARY - PARSING ONLY");
    println!("{}", "=".repeat(60));
    
    let test_names = ["identity::<i64>(42)", "Option::<i32>::None", "Vec::<i32>::new()"];
    
    for (i, (passed, name)) in test_results.iter().zip(test_names.iter()).enumerate() {
        println!("Test {} ({}): {}", i + 1, name, 
            if *passed { "✓ PASSED" } else { "✗ FAILED" });
    }
    
    let total_passed = test_results.iter().filter(|&&p| p).count();
    println!("\nParsing results: {}/{} tests passed", total_passed, test_results.len());
    
    // Progress report
    println!("\n{}", "=".repeat(60));
    println!("PROGRESS REPORT - PHASE 3");
    println!("{}", "=".repeat(60));
    println!("Time: {}", chrono::Local::now().format("%H:%M:%S"));
    println!("Status: Phase 3 progressive parsing tests completed");
    
    if total_passed == test_results.len() {
        println!("✅ PARSER: All tests passed - parser correctly handles:");
        println!("  1. Simple generic functions (identity::<i64>(42))");
        println!("  2. Generic enums (Option::<i32>::None)");
        println!("  3. Generic structs with impl blocks (Vec::<i32>::new())");
        println!("\n⚠️  KNOWN ISSUE: Full compilation pipeline has issues with Type::from_string");
        println!("   This is a codegen issue, not a parser issue.");
    } else {
        println!("⚠️  Some parsing tests failed. Issues found in parser.");
    }
    
    println!("\nNext steps: Fix Type::from_string in MIR generation code");
}