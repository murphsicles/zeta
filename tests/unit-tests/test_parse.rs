use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = std::fs::read_to_string("test_module_system.z").unwrap();
    
    match parse_zeta(&code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully!");
            println!("Remaining: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            
            // Print first few AST nodes
            for (i, node) in ast.iter().enumerate().take(5) {
                println!("Node {}: {:?}", i, node);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}