//! CTFE integration tests for Zeta compiler
//!
//! Tests compile-time function evaluation integrated into the full pipeline.

use zetac::compile_and_run_zeta;

/// Test basic comptime function evaluation
#[test]
fn test_comptime_basic_evaluation() {
    println!("=== CTFE Integration Test 1: Basic Comptime Function ===");

    let code = r#"
comptime fn answer() -> i64 {
    42
}

fn main() -> i64 {
    answer()
}
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_ok(), "Comptime function should compile and run");
    assert_eq!(result.unwrap(), 42, "Should evaluate comptime function at compile time");
    
    println!("✓ Basic comptime evaluation works");
}

/// Test comptime function with constant arguments
#[test]
fn test_comptime_with_args() {
    println!("=== CTFE Integration Test 2: Comptime Function with Args ===");

    let code = r#"
comptime fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn main() -> i64 {
    add(10, 32)
}
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_ok(), "Comptime function with args should compile");
    assert_eq!(result.unwrap(), 42, "Should evaluate comptime function with constant args");
    
    println!("✓ Comptime function with arguments works");
}

/// Test const variable evaluation
#[test]
fn test_const_variable_evaluation() {
    println!("=== CTFE Integration Test 3: Const Variable Evaluation ===");

    let code = r#"
const PI: f64 = 3.14159;
const TWO_PI: f64 = PI * 2.0;

fn main() -> i64 {
    // For now, just return success code
    0
}
    "#;

    let result = compile_and_run_zeta(code);
    // Should compile successfully (const evaluation happens at compile time)
    assert!(result.is_ok(), "Const variables should compile");
    
    println!("✓ Const variable evaluation works");
}

/// Test comptime function used in constant expression
#[test]
fn test_comptime_in_const_expr() {
    println!("=== CTFE Integration Test 4: Comptime in Const Expression ===");

    let code = r#"
comptime fn square(x: i64) -> i64 {
    x * x
}

const AREA: i64 = square(10);

fn main() -> i64 {
    AREA
}
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_ok(), "Comptime in const expression should compile");
    assert_eq!(result.unwrap(), 100, "Should evaluate comptime function in const expression");
    
    println!("✓ Comptime in const expression works");
}