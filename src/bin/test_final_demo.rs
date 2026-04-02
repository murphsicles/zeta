//! Final demo of comptime capabilities

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;

fn test_function(code: &str, func_name: &str, expected: i64) -> bool {
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            if !remaining.trim().is_empty() {
                println!("❌ {}: Partial parse", func_name);
                return false;
            }
            
            let mut evaluator = ConstEvaluator::new();
            if let Some(func) = ast.iter().find(|node| {
                if let AstNode::FuncDef { name, .. } = node {
                    name == func_name
                } else {
                    false
                }
            }) {
                match evaluator.try_eval_const_call(func, &[]) {
                    Ok(Some(ConstValue::Int(value))) => {
                        if value == expected {
                            println!("✅ {}: evaluates to {} (expected {})", func_name, value, expected);
                            true
                        } else {
                            println!("❌ {}: expected {}, got {}", func_name, expected, value);
                            false
                        }
                    }
                    Ok(Some(other)) => {
                        println!("❌ {}: evaluates to non-integer: {:?}", func_name, other);
                        false
                    }
                    Ok(None) => {
                        println!("❌ {}: does not evaluate", func_name);
                        false
                    }
                    Err(e) => {
                        println!("❌ {}: evaluation error: {}", func_name, e);
                        false
                    }
                }
            } else {
                println!("❌ {}: function not found", func_name);
                false
            }
        }
        Err(e) => {
            println!("❌ {}: parse error: {:?}", func_name, e);
            false
        }
    }
}

fn main() {
    println!("=== FINAL DEMO: Comptime Capabilities ===\n");
    
    let mut passed = 0;
    let mut total = 0;
    
    // Test 1: Variable assignment
    total += 1;
    if test_function(
        r#"comptime fn test() -> i64 { let a = 10; let b = 20; a + b }"#,
        "test",
        30,
    ) {
        passed += 1;
    }
    
    // Test 2: If statement
    total += 1;
    if test_function(
        r#"comptime fn test() -> i64 { let x = 5; let y = 10; if x < y { x + y } else { x - y } }"#,
        "test",
        15,
    ) {
        passed += 1;
    }
    
    // Test 3: Array indexing
    total += 1;
    if test_function(
        r#"comptime fn test() -> i64 { let arr = [1, 2, 3, 4, 5]; arr[2] }"#,
        "test",
        3,
    ) {
        passed += 1;
    }
    
    // Test 4: Nested blocks
    total += 1;
    if test_function(
        r#"comptime fn test() -> i64 { let outer = 100; { let inner = 42; outer + inner } }"#,
        "test",
        142,
    ) {
        passed += 1;
    }
    
    // Test 5: Binary operations
    total += 1;
    if test_function(
        r#"comptime fn test() -> i64 { (10 + 20) * 2 }"#,
        "test",
        60,
    ) {
        passed += 1;
    }
    
    // Test 6: Comparison operators
    total += 1;
    if test_function(
        r#"comptime fn test() -> i64 { if 5 < 10 { 100 } else { 200 } }"#,
        "test",
        100,
    ) {
        passed += 1;
    }
    
    println!("\n=== SUMMARY ===");
    println!("Passed: {}/{}", passed, total);
    println!("Success rate: {:.1}%", (passed as f32 / total as f32) * 100.0);
    
    if passed == total {
        println!("\n🎉 ALL TESTS PASS! Comptime evaluation is working!");
    } else {
        println!("\n⚠️  Some tests failed. Comptime evaluation is partially working.");
        std::process::exit(1);
    }
}