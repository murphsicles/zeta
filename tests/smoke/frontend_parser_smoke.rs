// Frontend Parser (SYN) Smoke Tests
// Basic smoke tests for the parser system

use zetac::frontend::parser::top_level::parse_zeta;

#[test]
fn test_parser_smoke_basic_expressions() {
    // Test that basic expressions parse
    let test_cases = vec![
        ("42", "integer literal"),
        ("3.14", "float literal"),
        ("\"hello\"", "string literal"),
        ("x", "identifier"),
        ("x + y", "binary operation"),
        ("-x", "unary operation"),
        ("f(x, y)", "function call"),
        ("x.y", "field access"),
    ];

    for (code, description) in test_cases {
        let result = parse_zeta(code);
        assert!(
            result.is_ok(),
            "Failed to parse {}: {:?}",
            description,
            result
        );
    }
}

#[test]
fn test_parser_smoke_statements() {
    // Test that basic statements parse
    let test_cases = vec![
        ("let x = 42;", "variable declaration"),
        ("x = 42;", "assignment"),
        ("return x;", "return statement"),
        ("if x { y }", "if statement"),
        ("while x { y }", "while loop"),
    ];

    for (code, description) in test_cases {
        let result = parse_zeta(code);
        assert!(
            result.is_ok(),
            "Failed to parse {}: {:?}",
            description,
            result
        );
    }
}

#[test]
fn test_parser_smoke_functions() {
    // Test that function definitions parse
    let code = r#"
        fn add(a: i32, b: i32) -> i32 {
            a + b
        }
    "#;

    let result = parse_zeta(code);
    assert!(
        result.is_ok(),
        "Failed to parse function definition: {:?}",
        result
    );
}

#[test]
fn test_parser_smoke_error_recovery() {
    // Test that parser handles errors gracefully
    // Note: This is a basic test - more comprehensive error recovery tests needed
    let code = "let x = ; let y = 42;";
    let result = parse_zeta(code);

    // Parser should handle the error and continue parsing
    // The exact behavior depends on error recovery implementation
    // For now, just verify it doesn't panic
    println!("Parser result for code with error: {:?}", result);
}

#[test]
fn test_parser_smoke_complex_expression() {
    // Test parsing a complex expression
    let code = "a + b * c - d / e % f";
    let result = parse_zeta(code);
    assert!(
        result.is_ok(),
        "Failed to parse complex expression: {:?}",
        result
    );
}
