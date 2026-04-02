//! Debug AST

use zetac::frontend::parser::top_level::parse_zeta;
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
        if let AstNode::FuncDef { name, body, ret_expr, .. } = node {
            if name == "test" {
                println!("Found test function");
                println!("Body has {} statements:", body.len());
                for (i, stmt) in body.iter().enumerate() {
                    println!("  Statement {}: {:?}", i, stmt);
                }
                if let Some(expr) = ret_expr {
                    println!("Return expression: {:?}", expr);
                }
                break;
            }
        }
    }
}