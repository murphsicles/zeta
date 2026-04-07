use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("Testing const declaration parsing...");
    
    // Test 1: Simple const declaration
    let code1 = r#"const MAX_LIMIT: usize = 1000000;"#;
    
    println!("\nTest 1: Simple const declaration");
    match parse_zeta(code1) {
        Ok((remaining, ast)) => {
            println!("Success! AST: {:?}", ast);
            if remaining.is_empty() {
                println!("✓ All input consumed");
            } else {
                println!("✗ Input not fully consumed, remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("✗ Parse error: {:?}", e);
        }
    }
    
    // Test 2: Const with expression
    let code2 = r#"const N: usize = 100;"#;
    
    println!("\nTest 2: Const with simple value");
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            println!("Success! AST: {:?}", ast);
            if remaining.is_empty() {
                println!("✓ All input consumed");
            } else {
                println!("✗ Input not fully consumed, remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("✗ Parse error: {:?}", e);
        }
    }
    
    // Test 3: Multiple const declarations
    let code3 = r#"
const MAX_LIMIT: usize = 1000000;
const N: usize = 100;
"#;
    
    println!("\nTest 3: Multiple const declarations");
    match parse_zeta(code3) {
        Ok((remaining, ast)) => {
            println!("Success! AST: {:?}", ast);
            if remaining.is_empty() {
                println!("✓ All input consumed");
            } else {
                println!("✗ Input not fully consumed, remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("✗ Parse error: {:?}", e);
        }
    }
}