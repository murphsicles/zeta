//! Test single for loop

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;
use std::fs;

fn main() {
    let filename = "tests/comptime-completion/test_one_iteration_i1.z";
    
    println!("=== Testing {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .expect("Failed to read file");
    
    let (remaining, ast) = parse_zeta(&content).expect("Parse failed");
    assert!(remaining.trim().is_empty(), "Partial parse: {}", remaining);
    
    println!("✅ Parses successfully");
    
    // Create evaluator
    let mut evaluator = ConstEvaluator::new();
    
    // Try to evaluate main
    for node in &ast {
        if let AstNode::FuncDef { name, .. } = node {
            if name == "main" {
                println!("  Found main function");
                match evaluator.try_eval_const_call(node, &[]) {
                    Ok(Some(ConstValue::Int(value))) => {
                        println!("✅ Main evaluates to: {}", value);
                        return;
                    }
                    Ok(Some(other)) => {
                        println!("❌ Main evaluates to non-integer: {:?}", other);
                        return;
                    }
                    Ok(None) => {
                        println!("❌ Main does not evaluate (returns None)");
                        return;
                    }
                    Err(e) => {
                        println!("❌ Evaluation error: {}", e);
                        return;
                    }
                }
            }
        }
    }
    
    println!("❌ No main function found");
}