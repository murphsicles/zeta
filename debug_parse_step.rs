use std::fs;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = fs::read_to_string("bootstrap/minimal_compiler.z").unwrap();
    
    // Parse the entire file
    match parse_zeta(&code) {
        Ok((remaining, asts)) => {
            println!("Parsed {} AST nodes", asts.len());
            println!("Remaining length: {}", remaining.len());
            
            if !remaining.is_empty() {
                // Find where the remaining text starts in the original
                let start_pos = code.len() - remaining.len();
                println!("Remaining starts at position: {}", start_pos);
                
                // Show context around where parsing stopped
                let context_start = start_pos.saturating_sub(100);
                let context_end = (start_pos + 100).min(code.len());
                println!("Context around where parsing stopped:");
                println!("{}", &code[context_start..context_end]);
                
                // Try to parse just the remaining part
                println!("\nTrying to parse remaining part:");
                match parse_zeta(&remaining) {
                    Ok((remaining2, asts2)) => {
                        println!("  Parsed {} AST nodes from remaining", asts2.len());
                        println!("  Still remaining: {} chars", remaining2.len());
                    }
                    Err(e) => {
                        println!("  Failed to parse remaining: {:?}", e);
                    }
                }
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}