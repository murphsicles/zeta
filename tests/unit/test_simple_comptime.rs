use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test simple comptime function without array type
    let code = r#"
comptime fn simple() -> i64 {
    return 42
}
    "#;
    
    println!("Testing simple comptime function:");
    println!("Code: {}", code);
    
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            if remaining.trim().is_empty() {
                println!("✅ Success! Full parse.");
                println!("AST nodes: {}", ast.len());
                for node in &ast {
                    println!("  {:?}", node);
                }
            } else {
                println!("❌ Partial parse. Remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}