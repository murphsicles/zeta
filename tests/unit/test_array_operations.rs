extern crate zetac;

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};

fn main() {
    println!("=== Testing array operations in comptime ===");
    
    // Test 1: Array literal
    let code1 = r#"
comptime fn test_array_literal() -> [3]i64 {
    [1, 2, 3]
}
    "#;
    
    println!("\nTest 1: Array literal");
    test_comptime_function(code1, "test_array_literal");
    
    // Test 2: Array indexing
    let code2 = r#"
comptime fn test_array_indexing() -> i64 {
    let arr = [10, 20, 30]
    arr[1]
}
    "#;
    
    println!("\nTest 2: Array indexing");
    test_comptime_function(code2, "test_array_indexing");
    
    // Test 3: Array element assignment
    let code3 = r#"
comptime fn test_array_assignment() -> [3]i64 {
    let mut arr = [1, 2, 3]
    arr[1] = 99
    arr
}
    "#;
    
    println!("\nTest 3: Array element assignment");
    test_comptime_function(code3, "test_array_assignment");
    
    // Test 4: Array return from function
    let code4 = r#"
comptime fn make_array() -> [3]i64 {
    [100, 200, 300]
}

comptime fn test_array_return() -> i64 {
    let arr = make_array()
    arr[2]
}
    "#;
    
    println!("\nTest 4: Array return from function");
    test_comptime_function(code4, "test_array_return");
}

fn test_comptime_function(code: &str, func_name: &str) {
    match parse_zeta(code) {
        Ok((_remaining, ast)) => {
            println!("  ✅ Parsed successfully");
            
            // Create evaluator and register all functions
            let mut evaluator = ConstEvaluator::new();
            
            // Register all const/comptime functions
            for node in &ast {
                if let zetac::frontend::ast::AstNode::FuncDef {
                    ref name,
                    const_,
                    comptime_,
                    ..
                } = *node {
                    if const_ || comptime_ {
                        evaluator.register_function(name.clone(), node.clone());
                    }
                }
            }
            
            // Find the test function
            let test_func = ast.iter().find(|node| {
                match node {
                    &zetac::frontend::ast::AstNode::FuncDef { name, .. } => {
                        name == func_name
                    }
                    _ => false,
                }
            });
            
            if let Some(func) = test_func {
                match evaluator.try_eval_const_call(func, &[]) {
                    Ok(Some(value)) => {
                        println!("  ✅ Function evaluates to: {:?}", value);
                    }
                    Ok(None) => {
                        println!("  ⚠️  Function doesn't evaluate yet");
                    }
                    Err(e) => {
                        println!("  ❌ Evaluation error: {}", e);
                    }
                }
            } else {
                println!("  ⚠️  No {} function found", func_name);
            }
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
}