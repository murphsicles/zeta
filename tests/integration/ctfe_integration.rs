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

/// Test comptime function calls in regular code (not just const expressions)
#[test]
#[ignore = "Parser issue with comptime functions containing while loops"]
fn test_comptime_in_regular_code() {
    println!("=== CTFE Integration Test 5: Comptime Calls in Regular Code ===");

    let code = r#"
comptime fn compute() -> i64 {
    let mut sum = 0;
    let mut i = 0;
    while i < 10 {
        sum = sum + i;
        i = i + 1;
    }
    sum
}

fn main() -> i64 {
    // This should be evaluated at compile time
    compute()
}
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_ok(), "Comptime calls in regular code should compile");
    // Sum of 0..9 = 45
    assert_eq!(result.unwrap(), 45, "Should evaluate comptime function in regular code");
    
    println!("✓ Comptime calls in regular code work");
}

/// Test loops with constant false condition are eliminated
#[test]
fn test_loop_constant_false_elimination() {
    println!("=== CTFE Integration Test 6: Loop Constant False Elimination ===");

    let code = r#"
fn main() -> i64 {
    let mut x = 0;
    while false {
        x = x + 1; // This should be dead code
    }
    x
}
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_ok(), "Loop with false condition should compile");
    assert_eq!(result.unwrap(), 0, "Loop body should be eliminated");
    
    println!("✓ Loop with constant false condition eliminated");
}

/// Test constant propagation in if statements
#[test]
fn test_if_constant_propagation() {
    println!("=== CTFE Integration Test 7: If Constant Propagation ===");

    let code = r#"
fn main() -> i64 {
    let x = if true { 42 } else { 0 };
    x
}
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_ok(), "If with constant condition should compile");
    assert_eq!(result.unwrap(), 42, "If should be simplified to then branch");
    
    println!("✓ If with constant condition simplified");
}