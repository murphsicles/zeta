//! Test basic comptime function evaluation

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::ConstEvaluator;

fn main() {
    println!("=== Testing basic comptime function evaluation ===\n");

    // Test 1: Simple comptime function with literal return
    let test1 = r#"
comptime fn answer() -> i64 {
    42
}
    "#;

    println!("Test 1: Simple comptime function with literal return");
    match parse_zeta(test1) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                
                // Try to evaluate
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(value) => {
                            println!("✅ Evaluates to: {}", value);
                            // Note: value is ConstValue, not i64
                            if let zetac::middle::const_eval::ConstValue::Int(int_value) = value {
                                assert_eq!(int_value, 42, "Expected 42, got {}", int_value);
                            } else {
                                println!("❌ Expected integer, got {:?}", value);
                            }
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

    println!("\n---\n");

    // Test 2: Comptime function with arithmetic
    let test2 = r#"
comptime fn add() -> i64 {
    10 + 32
}
    "#;

    println!("Test 2: Comptime function with arithmetic");
    match parse_zeta(test2) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                
                // Try to evaluate
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(value) => {
                            println!("✅ Evaluates to: {}", value);
                            if let zetac::middle::const_eval::ConstValue::Int(int_value) = value {
                                assert_eq!(int_value, 42, "Expected 42, got {}", int_value);
                            } else {
                                println!("❌ Expected integer, got {:?}", value);
                            }
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

    println!("\n---\n");

    // Test 3: Comptime function with return statement
    let test3 = r#"
comptime fn with_return() -> i64 {
    return 42
}
    "#;

    println!("Test 3: Comptime function with return statement");
    match parse_zeta(test3) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                
                // Try to evaluate
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(value) => {
                            println!("✅ Evaluates to: {}", value);
                            if let zetac::middle::const_eval::ConstValue::Int(int_value) = value {
                                assert_eq!(int_value, 42, "Expected 42, got {}", int_value);
                            } else {
                                println!("❌ Expected integer, got {:?}", value);
                            }
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

    println!("\n=== All tests completed ===");
}