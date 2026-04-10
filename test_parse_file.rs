use std::fs;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = fs::read_to_string("quick_test.z").expect("Failed to read file");
    
    println!("Parsing code:\n{}", code);
    println!("=" .repeat(50));
    
    match parse_zeta(&code) {
        Ok((remaining, ast)) => {
            println!("Success!");
            println!("Remaining: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            for (i, node) in ast.iter().enumerate() {
                println!("Node {}: {:?}", i, node);
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}