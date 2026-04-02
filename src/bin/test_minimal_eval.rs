//! Test minimal comptime evaluation

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::const_eval::{ConstEvaluator, ConstValue};
use zetac::AstNode;
use std::fs;

fn test_eval(filename: &str) -> Result<(), String> {
    println!("\n=== Testing {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .map_err(|e| format!("Failed to read file {}: {}", filename, e))?;
    
    match parse_zeta(&content) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Parses successfully");
                
                // Create evaluator
                let mut evaluator = ConstEvaluator::new();
                
                // First, evaluate all comptime functions
                for node in &ast {
                    if let AstNode::FuncDef { name, comptime_, .. } = node {
                        if *comptime_ {
                            println!("  Found comptime function: {}", name);
                            match evaluator.try_eval_const_call(node, &[]) {
                                Ok(Some(value)) => {
                                    println!("    Evaluates to: {}", value);
                                }
                                Ok(None) => {
                                    println!("    Returns None");
                                }
                                Err(e) => {
                                    println!("    Error: {}", e);
                                }
                            }
                        }
                    }
                }
                
                // Now try to evaluate main
                for node in &ast {
                    if let AstNode::FuncDef { name, .. } = node {
                        if name == "main" {
                            println!("  Found main function");
                            match evaluator.try_eval_const_call(node, &[]) {
                                Ok(Some(ConstValue::Int(value))) => {
                                    println!("✅ Main evaluates to: {}", value);
                                    return Ok(());
                                }
                                Ok(Some(other)) => {
                                    return Err(format!("Main evaluates to non-integer: {:?}", other));
                                }
                                Ok(None) => {
                                    return Err("Main does not evaluate (returns None)".to_string());
                                }
                                Err(e) => {
                                    return Err(format!("Evaluation error: {}", e));
                                }
                            }
                        }
                    }
                }
                
                Err("No main function found".to_string())
            } else {
                Err(format!("Partial parse, remaining: {}", remaining))
            }
        }
        Err(e) => {
            Err(format!("Parse error: {:?}", e))
        }
    }
}

fn main() {
    println!("=== Minimal Evaluation Tests ===\n");
    
    let test_files = [
        "tests/comptime-completion/test_minimal.z",
        "tests/comptime-completion/test_simple_for.z",
    ];
    
    for file in &test_files {
        match test_eval(file) {
            Ok(_) => println!("✅ PASS: {}", file),
            Err(e) => println!("❌ FAIL: {} - {}", file, e),
        }
    }
}