//! Test extended comptime capabilities

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};

fn test_case(name: &str, code: &str, expected: Option<i64>) {
    println!("\nTest: {}", name);
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(Some(ConstValue::Int(value))) => {
                            println!("✅ Evaluates to: {}", value);
                            if let Some(expected_val) = expected {
                                if value == expected_val {
                                    println!("✅ Matches expected: {}", expected_val);
                                } else {
                                    println!("❌ Expected {}, got {}", expected_val, value);
                                }
                            }
                        }
                        Ok(Some(other)) => {
                            println!("❌ Evaluates to non-integer: {:?}", other);
                        }
                        Ok(None) => {
                            println!("❌ Does not evaluate (returns None)");
                        }
                        Err(e) => {
                            println!("❌ Evaluation error: {}", e);
                        }
                    }
                }
            } else {
                println!("❌ Partial parse");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}

fn main() {
    println!("=== Testing extended comptime capabilities ===\n");

    // Test 1: Simple variable assignment
    test_case(
        "Simple variable assignment",
        r#"
comptime fn simple_assignment() -> i64 {
    let x = 42
    x
}
    "#,
        Some(42),
    );

    // Test 2: Multiple assignments
    test_case(
        "Multiple assignments",
        r#"
comptime fn multiple_assignments() -> i64 {
    let a = 10
    let b = 32
    a + b
}
    "#,
        Some(42),
    );

    // Test 3: Variable mutation
    test_case(
        "Variable mutation",
        r#"
comptime fn variable_mutation() -> i64 {
    var x = 5
    x = x + 10
    x
}
    "#,
        Some(15),
    );

    // Test 4: If statement with variables
    test_case(
        "If statement with variables",
        r#"
comptime fn if_with_vars() -> i64 {
    let x = 10
    let y = 20
    if x < y {
        x + y
    } else {
        x - y
    }
}
    "#,
        Some(30),
    );

    // Test 5: Array literal
    test_case(
        "Array literal",
        r#"
comptime fn array_literal() -> i64 {
    let arr = [1, 2, 3, 4, 5]
    arr[2]
}
    "#,
        Some(3),
    );

    // Test 6: Array element assignment
    test_case(
        "Array element assignment",
        r#"
comptime fn array_assignment() -> i64 {
    var arr = [0, 0, 0, 0, 0]
    arr[2] = 42
    arr[2]
}
    "#,
        Some(42),
    );

    // Test 7: Nested blocks
    test_case(
        "Nested blocks",
        r#"
comptime fn nested_blocks() -> i64 {
    let outer = 100
    {
        let inner = 42
        outer + inner
    }
}
    "#,
        Some(142),
    );

    // Test 8: Return statement
    test_case(
        "Return statement",
        r#"
comptime fn with_return() -> i64 {
    let x = 10
    let y = 20
    return x + y
}
    "#,
        Some(30),
    );

    println!("\n=== All tests completed ===");
}