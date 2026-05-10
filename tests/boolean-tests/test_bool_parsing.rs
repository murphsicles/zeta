use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let test_code = r#"
fn test_bool() -> bool {
    return true
}

fn main() -> u64 {
    return 42
}
"#;
    
    match parse_zeta(test_code) {
        Ok((remaining, ast)) => {
            println!("Parsing successful!");
            println!("Remaining input: '{}'", remaining);
            println!("AST nodes: {}", ast.len());
            for node in ast {
                println!("  {:?}", node);
            }
        }
        Err(e) => {
            println!("Parsing failed: {:?}", e);
        }
    }
}