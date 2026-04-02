//! Debug for loop evaluation

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;
use std::fs;

fn main() {
    let filename = "tests/comptime-completion/test_simple_for.z";
    
    println!("=== Debugging {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .expect("Failed to read file");
    
    let (remaining, ast) = parse_zeta(&content).expect("Parse failed");
    assert!(remaining.trim().is_empty(), "Partial parse: {}", remaining);
    
    println!("Parsed successfully");
    
    // Find the simple_for function
    for node in &ast {
        if let AstNode::FuncDef { name, .. } = node {
            if name == "simple_for" {
                println!("Found simple_for function");
                
                let mut evaluator = ConstEvaluator::new();
                
                // Add some debug prints by modifying the evaluator
                // For now, just call it
                match evaluator.try_eval_const_call(node, &[]) {
                    Ok(Some(value)) => {
                        println!("simple_for evaluates to: {}", value);
                    }
                    Ok(None) => {
                        println!("simple_for returns None");
                    }
                    Err(e) => {
                        println!("simple_for error: {}", e);
                    }
                }
                
                break;
            }
        }
    }
}