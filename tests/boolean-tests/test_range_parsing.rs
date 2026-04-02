use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== Testing Range Parsing ===\n");
    
    // Test 1: Exclusive range
    let test1 = r#"for i in 0..10 { println!("{}", i); }"#;
    println!("Test 1: Exclusive range '0..10'");
    match parse_zeta(test1) {
        Ok((remaining, ast)) => {
            println!("  ✅ Parsed successfully");
            println!("  AST: {:?}", ast);
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    
    // Test 2: Inclusive range
    let test2 = r#"for i in 0..=10 { println!("{}", i); }"#;
    println!("\nTest 2: Inclusive range '0..=10'");
    match parse_zeta(test2) {
        Ok((remaining, ast)) => {
            println!("  ✅ Parsed successfully");
            println!("  AST: {:?}", ast);
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    
    // Test 3: Range with step
    let test3 = r#"for i in (0..10).step(2) { println!("{}", i); }"#;
    println!("\nTest 3: Range with step '(0..10).step(2)'");
    match parse_zeta(test3) {
        Ok((remaining, ast)) => {
            println!("  ✅ Parsed successfully");
            println!("  AST: {:?}", ast);
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    
    // Test 4: Range expression as value
    let test4 = r#"let r = 1..10;"#;
    println!("\nTest 4: Range expression '1..10'");
    match parse_zeta(test4) {
        Ok((remaining, ast)) => {
            println!("  ✅ Parsed successfully");
            println!("  AST: {:?}", ast);
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
}