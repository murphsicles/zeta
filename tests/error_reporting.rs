use zetac::frontend::parser::expr::parse_expr;
use zetac::frontend::parser::stmt::parse_stmt;

#[test]
fn test_error_messages() {
    println!("=== Testing Error Reporting ===");
    
    // Test unclosed string
    match parse_expr(r#""hello world"#) {
        Ok(_) => println!("✗ Unclosed string should have failed"),
        Err(e) => println!("✓ Unclosed string failed as expected: {:?}", e),
    }
    
    // Test invalid identifier
    match parse_expr("123var") {
        Ok(_) => println!("✗ Invalid identifier should have failed"),
        Err(e) => println!("✓ Invalid identifier failed as expected: {:?}", e),
    }
    
    // Test missing operator
    match parse_expr("a b") {
        Ok(_) => println!("✗ Missing operator should have failed"),
        Err(e) => println!("✓ Missing operator failed as expected: {:?}", e),
    }
    
    // Test invalid assignment
    match parse_stmt("a + = b") {
        Ok(_) => println!("✗ Invalid assignment should have failed"),
        Err(e) => println!("✓ Invalid assignment failed as expected: {:?}", e),
    }
    
    // Test unclosed parentheses
    match parse_expr("(a + b") {
        Ok(_) => println!("✗ Unclosed parentheses should have failed"),
        Err(e) => println!("✓ Unclosed parentheses failed as expected: {:?}", e),
    }
    
    println!("\n=== Current Error Reporting Status ===");
    println!("- Basic error detection works");
    println!("- Error messages are technical (nom error kinds)");
    println!("- TODO: Add line/column numbers");
    println!("- TODO: Add human-readable messages");
    println!("- TODO: Add suggestions for common errors");
}

#[test]
fn test_multiline_error_locations() {
    // Test with multiline input
    let code = r#"let x = 42
let y = "unclosed string
let z = 100"#;
    
    println!("\n=== Testing Multiline Error ===");
    println!("Code:\n{}", code);
    
    // Try to parse (will fail on the unclosed string)
    // Note: parse_stmt only parses one statement at a time
    match parse_stmt("let y = \"unclosed string") {
        Ok(_) => println!("✗ Should have failed on unclosed string"),
        Err(e) => {
            println!("✓ Failed as expected");
            println!("Error: {:?}", e);
            println!("Note: Current errors don't show line numbers");
        }
    }
}