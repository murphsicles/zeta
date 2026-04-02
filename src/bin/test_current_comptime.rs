//! Test current comptime capabilities

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};

fn main() {
    println!("=== Testing current comptime capabilities ===\n");

    // Test 1: Simple variable assignment
    let test1 = r#"
comptime fn simple_assignment() -> i64 {
    let x = 42
    x
}
    "#;

    println!("Test 1: Simple variable assignment");
    match parse_zeta(test1) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(Some(ConstValue::Int(value))) => {
                            println!("✅ Evaluates to: {}", value);
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

    println!("\n---\n");

    // Test 2: If statement
    let test2 = r#"
comptime fn simple_if() -> i64 {
    if true {
        42
    } else {
        0
    }
}
    "#;

    println!("Test 2: Simple if statement");
    match parse_zeta(test2) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(Some(ConstValue::Int(value))) => {
                            println!("✅ Evaluates to: {}", value);
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

    println!("\n---\n");

    // Test 3: Array assignment
    let test3 = r#"
comptime fn array_indexing() -> i64 {
    let arr = [10, 20, 30, 40, 50]
    arr[2]
}
    "#;

    println!("Test 3: Array indexing");
    match parse_zeta(test3) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                
                let mut evaluator = ConstEvaluator::new();
                if let Some(func) = ast.first() {
                    match evaluator.try_eval_const_call(func, &[]) {
                        Ok(Some(ConstValue::Int(value))) => {
                            println!("✅ Evaluates to: {}", value);
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

    println!("\n=== All tests completed ===");
}