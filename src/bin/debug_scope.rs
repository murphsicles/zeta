//! Debug scope

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;
use std::fs;

fn main() {
    let filename = "tests/comptime-completion/test_scope.z";
    
    println!("=== Debugging {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .expect("Failed to read file");
    
    let (remaining, ast) = parse_zeta(&content).expect("Parse failed");
    assert!(remaining.trim().is_empty(), "Partial parse: {}", remaining);
    
    println!("Parsed successfully");
    
    // Find the test_scope function
    for node in &ast {
        if let AstNode::FuncDef { name, .. } = node {
            if name == "test_scope" {
                println!("Found test_scope function");
                
                let mut evaluator = ConstEvaluator::new();
                
                match evaluator.try_eval_const_call(node, &[]) {
                    Ok(Some(value)) => {
                        println!("test_scope evaluates to: {}", value);
                        // Should be 1
                    }
                    Ok(None) => {
                        println!("test_scope returns None");
                    }
                    Err(e) => {
                        println!("test_scope error: {}", e);
                    }
                }
                
                break;
            }
        }
    }
}