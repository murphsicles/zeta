use zetac::frontend::parser::expr::parse_expr;

fn main() {
    // Test integer literal
    match parse_expr("123") {
        Ok((remaining, ast)) => {
            println!("Integer test passed:");
            println!("  Remaining: '{}'", remaining);
            println!("  AST: {:?}", ast);
        }
        Err(e) => println!("Integer test failed: {:?}", e),
    }
    
    // Test float literal
    match parse_expr("123.456") {
        Ok((remaining, ast)) => {
            println!("Float test passed:");
            println!("  Remaining: '{}'", remaining);
            println!("  AST: {:?}", ast);
        }
        Err(e) => println!("Float test failed: {:?}", e),
    }
    
    // Test float with scientific notation
    match parse_expr("1.23e-4") {
        Ok((remaining, ast)) => {
            println!("Scientific notation test passed:");
            println!("  Remaining: '{}'", remaining);
            println!("  AST: {:?}", ast);
        }
        Err(e) => println!("Scientific notation test failed: {:?}", e),
    }
    
    // Test float ending with dot
    match parse_expr("123.") {
        Ok((remaining, ast)) => {
            println!("Float with trailing dot test passed:");
            println!("  Remaining: '{}'", remaining);
            println!("  AST: {:?}", ast);
        }
        Err(e) => println!("Float with trailing dot test failed: {:?}", e),
    }
}