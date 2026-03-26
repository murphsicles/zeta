extern crate zetac;

use zetac::frontend::ast::AstNode;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = r#"
// Zeta constant test file
// Demonstrates constant parsing and usage

// Simple integer constant
const MAX_BUFFER_SIZE: i64 = 4096;

// String constant
const APP_NAME: &str = "Zeta Compiler";

// Boolean constant
const DEBUG_MODE: bool = true;

// Computed constant
const DEFAULT_TIMEOUT: i64 = 30 * 1000; // 30 seconds in milliseconds

// Constant used in expression
const HEADER_SIZE: i64 = 128;
const TOTAL_SIZE: i64 = MAX_BUFFER_SIZE + HEADER_SIZE;

fn main() -> i64 {
    // Use constants in function
    if DEBUG_MODE {
        TOTAL_SIZE
    } else {
        MAX_BUFFER_SIZE
    }
}
"#;

    println!("Testing comprehensive constant parsing...");
    println!("=========================================\n");
    
    match parse_zeta(code) {
        Ok((remaining, asts)) => {
            println!("✓ Parsing successful!");
            println!("  AST nodes found: {}", asts.len());
            println!("  Characters remaining: {}", remaining.len());
            
            let mut const_count = 0;
            let mut func_count = 0;
            
            for (i, ast) in asts.iter().enumerate() {
                match ast {
                    AstNode::ConstDef { name, ty, .. } => {
                        println!("  Constant {}: {}: {}", const_count + 1, name, ty);
                        const_count += 1;
                    }
                    AstNode::FuncDef { name, .. } => {
                        println!("  Function: {}", name);
                        func_count += 1;
                    }
                    _ => {
                        println!("  Other: {:?}", ast);
                    }
                }
            }
            
            println!("\nSummary:");
            println!("  Total constants: {}", const_count);
            println!("  Total functions: {}", func_count);
            
            if const_count == 6 {
                println!("✓ All 6 constants parsed correctly!");
            } else {
                println!("✗ Expected 6 constants, found {}", const_count);
            }
            
            if func_count == 1 {
                println!("✓ Function parsed correctly!");
            }
            
            if remaining.is_empty() {
                println!("✓ All input consumed!");
            }
        }
        Err(e) => {
            println!("✗ Parsing failed: {:?}", e);
        }
    }
}