use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test 1: Basic comptime function
    let code1 = r#"
    comptime fn generate_residues() -> [NUM_RESIDUES]u64 {
        return [0; NUM_RESIDUES]
    }
    "#;
    
    println!("Test 1: Basic comptime function");
    match parse_zeta(code1) {
        Ok((remaining, ast)) => {
            println!("  Success! Remaining: '{}'", remaining);
            println!("  AST nodes: {}", ast.len());
            for node in ast {
                println!("    {:?}", node);
            }
        }
        Err(e) => println!("  Error: {:?}", e),
    }
    
    // Test 2: Multiple function types
    let code2 = r#"
    comptime fn comptime_func() -> i64 { return 1 }
    const fn const_func() -> i64 { return 2 }
    async fn async_func() -> i64 { return 3 }
    fn regular_func() -> i64 { return 4 }
    "#;
    
    println!("\nTest 2: Multiple function types");
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            println!("  Success! Remaining: '{}'", remaining);
            println!("  AST nodes: {}", ast.len());
        }
        Err(e) => println!("  Error: {:?}", e),
    }
    
    // Test 3: Pub comptime function
    let code3 = r#"
    pub comptime fn public_comptime() -> i64 {
        return 42
    }
    "#;
    
    println!("\nTest 3: Pub comptime function");
    match parse_zeta(code3) {
        Ok((remaining, ast)) => {
            println!("  Success! Remaining: '{}'", remaining);
            println!("  AST nodes: {}", ast.len());
        }
        Err(e) => println!("  Error: {:?}", e),
    }
}