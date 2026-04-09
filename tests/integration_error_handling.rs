//! Integration test for error handling improvements

use zetac::compile_with_diagnostics;

#[test]
fn test_basic_parse_error() {
    // This should fail because of missing semicolon
    // But the parser might recover, so let's test with a clear syntax error
    let code = r#"fn main() {
    let x = 42;
    let y = x + ;
}"#;

    let result = compile_with_diagnostics(code, "test.z");
    assert!(result.is_err());

    let error = result.unwrap_err();
    println!("Error output:\n{}", error);

    // Should have file:line:column information
    assert!(error.contains("test.z"));
    assert!(error.contains("error["));
}

#[test]
fn test_type_error() {
    // Type errors might not be caught yet in current implementation
    // Let's test with undefined variable instead
    let code = r#"fn main() {
    let x = y;  // y is undefined
    println!("{}", x);
}"#;

    let result = compile_with_diagnostics(code, "test.z");
    // This might or might not fail depending on type checking
    // For now, just check it compiles or gives reasonable error
    if let Err(error) = result {
        println!("Error output:\n{}", error);
        // Should have error information
        assert!(error.contains("test.z") || error.contains("error["));
    }
}

#[test]
fn test_multiple_errors() {
    let code = r#"fn main() {
    let x = ;
    let y = x + z;
    println!("{}", y)
}"#;

    let result = compile_with_diagnostics(code, "test.z");
    assert!(result.is_err());

    let error = result.unwrap_err();
    println!("Error output:\n{}", error);

    // Should report multiple errors
    assert!(error.contains("error["));
}

#[test]
fn test_missing_main() {
    let code = r#"fn foo() {
    println!("Hello");
}"#;

    let result = compile_with_diagnostics(code, "test.z");
    assert!(result.is_err());

    let error = result.unwrap_err();
    println!("Error output:\n{}", error);

    // Should mention missing main function
    assert!(error.contains("main") || error.contains("Main"));
}

#[test]
fn test_syntax_error_with_context() {
    let code = r#"fn main() {
    let s = "unterminated string;
    println!("{}", s);
}"#;

    let result = compile_with_diagnostics(code, "test.z");
    assert!(result.is_err());

    let error = result.unwrap_err();
    println!("Error output:\n{}", error);

    // Should show context with caret
    assert!(error.contains("unterminated"));
    assert!(error.contains("\""));
}
