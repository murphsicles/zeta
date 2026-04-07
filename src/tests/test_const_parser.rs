use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("Testing const declaration parsing...");
    
    // Test 1: Simple const declaration
    let code1 = r#"const N: usize = 100;"#;
    
    println!("\nTest 1: Simple const declaration");
    println!("Code: {}", code1);
    match parse_zeta(code1) {
        Ok((remaining, ast)) => {
            println!("  Success! AST has {} nodes", ast.len());
            if !remaining.trim().is_empty() {
                println!("  Remaining: '{}'", remaining);
            } else {
                println!("  All input consumed");
            }
            println!("  AST: {:?}", ast);
        }
        Err(e) => {
            println!("  Error: {:?}", e);
        }
    }
    
    // Test 2: Const with underscore
    let code2 = r#"const MAX_LIMIT: usize = 1_000_000;"#;
    
    println!("\nTest 2: Const with underscore");
    println!("Code: {}", code2);
    match parse_zeta(code2) {
        Ok((remaining, ast)) => {
            println!("  Success! AST has {} nodes", ast.len());
            if !remaining.trim().is_empty() {
                println!("  Remaining: '{}'", remaining);
            } else {
                println!("  All input consumed");
            }
            println!("  AST: {:?}", ast);
        }
        Err(e) => {
            println!("  Error: {:?}", e);
        }
    }
    
    // Test 3: Multiple const declarations
    let code3 = r#"
const N: usize = 100;
const M: i64 = 42;
"#;
    
    println!("\nTest 3: Multiple const declarations");
    println!("Code: {}", code3);
    match parse_zeta(code3) {
        Ok((remaining, ast)) => {
            println!("  Success! AST has {} nodes", ast.len());
            if !remaining.trim().is_empty() {
                println!("  Remaining: '{}'", remaining);
            } else {
                println!("  All input consumed");
            }
            println!("  AST: {:?}", ast);
        }
        Err(e) => {
            println!("  Error: {:?}", e);
        }
    }
}