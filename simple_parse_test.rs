fn main() {
    let code = r#"
use std::malloc
use std::free

fn main() -> i64 {
    let ptr = malloc(100);
    free(ptr);
    0
}
    "#;
    
    println!("Testing parse of: {}", code);
    
    // Try to parse it
    match zetac::frontend::parser::top_level::parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("✅ Parsed successfully!");
            println!("AST nodes: {}", ast.len());
            println!("Remaining: '{}'", remaining);
            
            // Check for use statements
            for node in &ast {
                if let zetac::frontend::ast::AstNode::Use { path } = node {
                    println!("Found use statement: {}", path.join("::"));
                }
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}