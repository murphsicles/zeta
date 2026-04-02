//! Test pattern matching parsing

use zetac::frontend::parser::top_level::parse_zeta;
use std::fs;

fn main() {
    let filename = "final_demo.z";
    
    println!("=== Testing pattern matching parsing from {} ===", filename);
    
    let content = fs::read_to_string(filename)
        .expect("Failed to read file");
    
    match parse_zeta(&content) {
        Ok((remaining, ast)) => {
            println!("Parsed successfully!");
            println!("Remaining: '{}'", remaining);
            println!("AST has {} nodes", ast.len());
            
            for (i, node) in ast.iter().enumerate() {
                println!("Node {}: {:?}", i, node);
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}