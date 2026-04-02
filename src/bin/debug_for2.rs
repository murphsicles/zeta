//! Debug for loop evaluation

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;
use std::fs;

fn main() {
    let filename = "tests/comptime-completion/test_debug_for.z";
    
    println!("=== Debugging {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .expect("Failed to read file");
    
    let (remaining, ast) = parse_zeta(&content).expect("Parse failed");
    assert!(remaining.trim().is_empty(), "Partial parse: {}", remaining);
    
    println!("Parsed successfully");
    
    // Find the test_for function
    for node in &ast {
        if let AstNode::FuncDef { name, .. } = node {
            if name == "test_for" {
                println!("Found test_for function");
                
                let mut evaluator = ConstEvaluator::new();
                
                match evaluator.try_eval_const_call(node, &[]) {
                    Ok(Some(value)) => {
                        println!("test_for evaluates to: {}", value);
                        // Should be 0+1+2+3+4 = 10
                    }
                    Ok(None) => {
                        println!("test_for returns None");
                    }
                    Err(e) => {
                        println!("test_for error: {}", e);
                    }
                }
                
                break;
            }
        }
    }
}