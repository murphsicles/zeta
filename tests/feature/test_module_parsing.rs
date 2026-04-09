use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test 1: Basic module
    let code1 = r#"
        mod math {
            pub fn add(a: i64, b: i64) -> i64 {
                a + b
            }
        }
        
        fn main() -> i64 {
            42
        }
    "#;
    
    println!("Test 1: Basic module parsing");
    match parse_zeta(code1) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Test 2: Module with use statement
    let code2 = r#"
        mod math {
            pub fn add(a: i64, b: i64) -> i64 {
                a + b
            }
        }
        
        use math::add;
        
        fn main() -> i64 {
            add(10, 20)
        }
    "#;
    
    println!("\nTest 2: Module with use statement");
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Test 3: Nested modules
    let code3 = r#"
        mod outer {
            pub mod inner {
                pub fn secret() -> i64 {
                    42
                }
            }
        }
        
        fn main() -> i64 {
            outer::inner::secret()
        }
    "#;
    
    println!("\nTest 3: Nested modules");
    match parse_zeta(code3) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
    
    // Test 4: Private function (not pub)
    let code4 = r#"
        mod math {
            fn private_add(a: i64, b: i64) -> i64 {
                a + b
            }
            
            pub fn public_add(a: i64, b: i64) -> i64 {
                private_add(a, b)
            }
        }
        
        fn main() -> i64 {
            math::public_add(5, 3)
        }
    "#;
    
    println!("\nTest 4: Public and private functions");
    match parse_zeta(code4) {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}