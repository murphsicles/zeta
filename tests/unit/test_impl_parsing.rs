use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = r#"
// Test simple impl block
impl Option<i64> {
    fn unwrap(self) -> i64 {
        match self {
            Some(x) => x,
            None => 0,
        }
    }
}

// Test generic impl block
impl<T> Option<T> {
    fn unwrap(self) -> T {
        match self {
            Some(x) => x,
            None => panic!("unwrap on None"),
        }
    }
}
"#;

    println!("Testing impl block parsing...");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("Parse successful!");
            println!("Remaining input length: {}", remaining.len());
            println!("AST nodes: {}", ast.len());
            
            for (i, node) in ast.iter().enumerate() {
                println!("Node {}: {:?}", i, node);
            }
        }
        Err(e) => {
            println!("Parse failed: {:?}", e);
        }
    }
}