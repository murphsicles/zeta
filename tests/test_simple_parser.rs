use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = r#"
        struct Vec<T> {
            data: [T],
        }
        
        impl<T> Vec<T> {
            fn new() -> Vec<T> {
                Vec { data: [] }
            }
            
            fn push(self, value: T) -> Vec<T> {
                // Implementation not important for this test
                self
            }
        }
        
        fn main() {
            let v = Vec::<i32>::new();
            let v2 = v.push(42);
        }
    "#;
    
    println!("Parsing code...");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("Success!");
            println!("Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}