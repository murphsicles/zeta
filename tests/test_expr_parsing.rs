use zetac::frontend::parser::expr::parse_expr;

fn test_expr(expr: &str, expected: &str) {
    println!("Testing: {}", expr);
    match parse_expr(expr) {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                println!("  ERROR: Unparsed input: '{}'", remaining);
            } else {
                println!("  Success: {:?}", ast);
                if format!("{:?}", ast).contains(expected) {
                    println!("  ✓ Contains expected: {}", expected);
                } else {
                    println!("  ✗ Does not contain expected: {}", expected);
                }
            }
        }
        Err(e) => {
            println!("  ERROR: Parse error: {:?}", e);
        }
    }
    println!();
}

fn main() {
    println!("Testing expression parsing with type arguments...\n");
    
    // Test cases from failing tests
    test_expr("Vec::<i32>::new()", "type_args: [\"i32\"]");
    test_expr("Option::<bool>::None", "type_args: [\"bool\"]");
    test_expr("Result::<i32, String>::Ok(42)", "type_args: [\"i32\", \"String\"]");
    
    // Basic static method calls
    test_expr("Point::new(10, 20)", "PathCall");
    test_expr("Vec::new()", "PathCall");
    test_expr("Option::None", "PathCall");
    test_expr("Result::Ok(42)", "PathCall");
    
    // Edge cases
    test_expr("::new()", "should fail");
    test_expr("Point::(10, 20)", "Var(\"Point\")");
}