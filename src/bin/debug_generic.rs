use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = r#"
    struct Vec<T> {
        data: [T],
    }
    
    fn vec_new<T>() -> Vec<T> {
        Vec { data: [] }
    }
    
    fn main() -> i64 {
        let v = vec_new::<i32>();
        0
    }
    "#;

    let result = parse_zeta(code);
    println!("Result: {:?}", result);

    match result {
        Ok((remaining, ast)) => {
            println!("Remaining: '{}'", remaining);
            println!("AST: {:#?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}
