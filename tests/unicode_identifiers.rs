use zetac::frontend::parser::expr::parse_expr;

#[test]
fn test_unicode_identifiers() {
    // Helper function to check if entire input is parsed as an expression
    fn parse_full(input: &str) -> bool {
        match parse_expr(input) {
            Ok(("", _)) => true,  // Entire input consumed
            _ => false,
        }
    }
    
    // Test ASCII identifiers
    assert!(parse_full("my_var"));
    assert!(parse_full("_private"));
    assert!(parse_full("var123"));
    
    // Test Greek letters
    assert!(parse_full("π"));
    assert!(parse_full("α"));
    assert!(parse_full("β"));
    assert!(parse_full("γ"));
    
    // Test Japanese identifiers
    assert!(parse_full("変数"));
    assert!(parse_full("名前"));
    
    // Test Chinese identifiers
    assert!(parse_full("标识符"));
    assert!(parse_full("变量"));
    
    // Test mixed Unicode identifiers
    assert!(parse_full("π_approx"));
    assert!(parse_full("αβγ123"));
    assert!(parse_full("変数_name"));
    
    // Test that keywords still don't work
    assert!(!parse_full("let"));
    assert!(!parse_full("if"));
    assert!(!parse_full("fn"));
    
    // Test that identifiers can't start with digits
    // Note: "123var" will parse "123" as integer literal, leaving "var"
    // So it won't parse the entire input as a single expression
    assert!(!parse_full("123var"));
}

#[test]
fn test_unicode_in_expressions() {
    // Test Unicode identifiers in expressions
    assert!(parse_expr("π + α").is_ok());
    assert!(parse_expr("変数 * 2").is_ok());
    assert!(parse_expr("标识符 == 标识符").is_ok());
}