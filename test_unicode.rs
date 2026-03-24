use zetac::frontend::parser::expr::parse_expr;

fn test_identifier(input: &str, should_work: bool) {
    println!("\nTesting identifier: '{}'", input);
    match parse_expr(input) {
        Ok((remaining, ast)) => {
            if should_work {
                println!("  ✓ Success as expected");
                println!("  Remaining: '{}'", remaining);
                println!("  AST: {:?}", ast);
            } else {
                println!("  ✗ Unexpected success - should have failed");
            }
        }
        Err(e) => {
            if !should_work {
                println!("  ✓ Failed as expected: {:?}", e);
            } else {
                println!("  ✗ Unexpected failure: {:?}", e);
            }
        }
    }
}

fn main() {
    println!("=== Testing Unicode Identifier Support ===");
    
    // Test ASCII identifiers (should work)
    test_identifier("my_var", true);
    test_identifier("_private", true);
    test_identifier("var123", true);
    
    // Test Greek letters (should work with Unicode support)
    test_identifier("π", true);
    test_identifier("α", true);
    test_identifier("β", true);
    test_identifier("γ", true);
    
    // Test Japanese identifiers
    test_identifier("変数", true);
    test_identifier("名前", true);
    
    // Test Chinese identifiers
    test_identifier("标识符", true);
    test_identifier("变量", true);
    
    // Test mixed Unicode identifiers
    test_identifier("π_approx", true);
    test_identifier("αβγ123", true);
    test_identifier("変数_name", true);
    
    // Test emoji identifiers (should work - they're valid XID_Continue after first char)
    test_identifier("var😀", true);  // 😀 is XID_Continue
    test_identifier("π😎", true);    // 😎 is XID_Continue
    
    // Test invalid identifiers (should fail)
    test_identifier("123var", false);  // Can't start with digit
    test_identifier("", false);        // Empty
    test_identifier("let", false);     // Keyword
    test_identifier("if", false);      // Keyword
    
    println!("\n=== Test Complete ===");
}