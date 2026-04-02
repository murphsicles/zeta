//! Debug assignment

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;
use std::fs;

fn main() {
    let filename = "tests/comptime-completion/test_simple_assign.z";
    
    println!("=== Debugging {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .expect("Failed to read file");
    
    let (remaining, ast) = parse_zeta(&content).expect("Parse failed");
    assert!(remaining.trim().is_empty(), "Partial parse: {}", remaining);
    
    println!("Parsed successfully");
    
    // Find the test_assign function
    for node in &ast {
        if let AstNode::FuncDef { name, .. } = node {
            if name == "test_assign" {
                println!("Found test_assign function");
                
                let mut evaluator = ConstEvaluator::new();
                
                match evaluator.try_eval_const_call(node, &[]) {
                    Ok(Some(value)) => {
                        println!("test_assign evaluates to: {}", value);
                        // Should be 2
                    }
                    Ok(None) => {
                        println!("test_assign returns None");
                    }
                    Err(e) => {
                        println!("test_assign error: {}", e);
                    }
                }
                
                break;
            }
        }
    }
}