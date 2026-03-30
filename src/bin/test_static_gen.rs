use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let code = r#"
        struct Point { x: i64, y: i64 }
        
        // Standalone function (not in impl block)
        fn point_new(x: i64, y: i64) -> Point { 
            Point { x, y } 
        }
        
        fn main() -> i64 { 
            let p = point_new(10, 20);
            p.x + p.y
        }
    "#;

    println!("Testing with standalone function...");
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                println!("Error: Unparsed input: '{}'", remaining);
                return;
            }
            println!("Parse successful!");
            println!("AST has {} nodes", ast.len());
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }

    println!("\n---\n");

    let code2 = r#"
        struct Point { x: i64, y: i64 }
        
        impl Point {
            fn new(x: i64, y: i64) -> Point { 
                Point { x, y } 
            }
        }
        
        fn main() -> i64 { 
            let p = Point::new(10, 20);
            p.x + p.y
        }
    "#;

    println!("Testing with static method in impl block...");
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                println!("Error: Unparsed input: '{}'", remaining);
                return;
            }
            println!("Parse successful!");
            println!("AST has {} nodes", ast.len());

            // Debug: print the AST structure
            for (i, node) in ast.iter().enumerate() {
                println!("Node {}: {:?}", i, node);
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}
