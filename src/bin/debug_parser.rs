use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = r#"
    fn main() {
        let x = ::new();
    }
    "#;

    let result = parse_zeta(code);
    println!("Result: {:?}", result);

    match result {
        Ok((remaining, ast)) => {
            println!("Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}
