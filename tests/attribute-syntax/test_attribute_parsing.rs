use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = std::fs::read_to_string("test_attribute_syntax.z").unwrap();
    
    println!("=== Testing Attribute Syntax Parsing ===\n");
    println!("Code to parse:");
    println!("{}", code);
    println!("\n--- Parsing Results ---");
    
    match parse_zeta(&code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully!");
            println!("Remaining: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            
            // Print AST nodes with attributes
            for (i, node) in ast.iter().enumerate() {
                println!("\nNode {}: {:?}", i, node);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}