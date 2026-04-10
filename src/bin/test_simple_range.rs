use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== Testing Simple Range Loop Parsing ===\n");

    let code = r#"
fn main() -> i64 {
    let mut sum = 0;
    for i in 1..10 {
        sum = sum + i;
    }
    return sum;
}
"#;

    println!("Code:\n{}", code);
    
    match parse_zeta(code) {
        Ok((remaining, asts)) => {
            println!("\nParsed successfully!");
            println!("Remaining: '{}'", remaining);
            println!("AST count: {}", asts.len());
            
            for (i, ast) in asts.iter().enumerate() {
                println!("\nAST {}: {:?}", i, ast);
            }
        }
        Err(e) => {
            println!("\nParse error: {:?}", e);
        }
    }
}