use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = r#"
comptime fn test() -> i64 {
    return 42
}

comptime const_value: i64 = 123
"#;
    
    println!("Testing comptime parsing...");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            for node in ast {
                println!("  {:?}", node);
            }
        }
        Err(e) => println!("Error: {:?}", e),
    }
}