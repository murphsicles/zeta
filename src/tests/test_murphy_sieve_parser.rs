use zetac::frontend::parser::top_level::parse_zeta;
use std::fs;

fn main() {
    println!("Testing Murphy's Sieve const declaration parsing...");
    
    // Read the actual Murphy's Sieve file
    let code = fs::read_to_string("src/murphy_sieve_true.z").unwrap();
    
    println!("Parsing file: src/murphy_sieve_true.z");
    println!("First few lines:\n{}", code.lines().take(10).collect::<Vec<_>>().join("\n"));
    
    match parse_zeta(&code) {
        Ok((remaining, ast)) => {
            println!("\nSuccess! AST has {} top-level nodes", ast.len());
            
            // Count const declarations
            let const_count = ast.iter().filter(|node| {
                match node {
                    zetac::frontend::ast::AstNode::ConstDef { .. } => true,
                    _ => false,
                }
            }).count();
            
            println!("Found {} const declarations", const_count);
            
            // Show const declarations
            for (i, node) in ast.iter().enumerate() {
                match node {
                    zetac::frontend::ast::AstNode::ConstDef { name, ty, value, .. } => {
                        println!("  Const {}: {}: {} = {:?}", i, name, ty, value);
                    }
                    zetac::frontend::ast::AstNode::FuncDef { name, .. } => {
                        println!("  Function {}: {}", i, name);
                    }
                    _ => {
                        println!("  Other node {}: {:?}", i, node);
                    }
                }
            }
            
            if !remaining.trim().is_empty() {
                println!("\nRemaining input (first 200 chars): '{}'", &remaining[..remaining.len().min(200)]);
            } else {
                println!("\nAll input consumed");
            }
        }
        Err(e) => {
            println!("\nParse error: {:?}", e);
        }
    }
}