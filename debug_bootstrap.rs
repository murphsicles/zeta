use std::fs;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = fs::read_to_string("bootstrap/minimal_compiler.z").unwrap();
    
    // Try to parse the entire file
    match parse_zeta(&code) {
        Ok((remaining, asts)) => {
            println!("Parsed {} AST nodes", asts.len());
            println!("Remaining length: {}", remaining.len());
            if !remaining.is_empty() {
                println!("First 200 chars of remaining:");
                println!("{}", &remaining[0..remaining.len().min(200)]);
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
    
    // Now try to parse just the part after the struct
    let code = fs::read_to_string("bootstrap/minimal_compiler.z").unwrap();
    // Find where the struct ends
    if let Some(struct_end) = code.find("impl Parser {") {
        let after_struct = &code[struct_end..];
        println!("\n\nTrying to parse from 'impl Parser {':");
        println!("First 200 chars: {}", &after_struct[0..after_struct.len().min(200)]);
        
        match parse_zeta(after_struct) {
            Ok((remaining, asts)) => {
                println!("Parsed {} AST nodes", asts.len());
                println!("Remaining length: {}", remaining.len());
            }
            Err(e) => {
                println!("Parse error: {:?}", e);
            }
        }
    }
}