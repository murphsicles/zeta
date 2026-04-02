//! Debug no var

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;
use std::fs;

fn main() {
    let filename = "tests/comptime-completion/test_no_var.z";
    
    println!("=== Debugging {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .expect("Failed to read file");
    
    let (remaining, ast) = parse_zeta(&content).expect("Parse failed");
    assert!(remaining.trim().is_empty(), "Partial parse: {}", remaining);
    
    println!("Parsed successfully");
    
    // Find the test function
    for node in &ast {
        if let AstNode::FuncDef { name, .. } = node {
            if name == "test" {
                println!("Found test function");
                
                let mut evaluator = ConstEvaluator::new();
                
                match evaluator.try_eval_const_call(node, &[]) {
                    Ok(Some(value)) => {
                        println!("test evaluates to: {}", value);
                    }
                    Ok(None) => {
                        println!("test returns None");
                    }
                    Err(e) => {
                        println!("test error: {}", e);
                    }
                }
                
                break;
            }
        }
    }
}