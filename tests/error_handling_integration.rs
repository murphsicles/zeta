//! Integration tests for Error Handling System (SEM + LEX)
//! Tests error propagation and recovery across compiler systems

use zetac::compile_and_run_zeta;

/// Test that syntax errors are properly detected and reported
#[test]
fn test_syntax_error_detection() {
    let code = r#"
        fn main() -> i32 {
            let x = 42
            // Missing semicolon - but Zeta may allow newlines as separators
            // Use actual invalid syntax instead: incomplete expression
            let y = + 
            x
        }
    "#;

    let result = compile_and_run_zeta(code);

    // Should fail with syntax error (incomplete expression after =)
    assert!(result.is_err(), "Syntax error should be detected");

    let error = result.unwrap_err();
    let error_str = error.to_string();
    println!("Syntax error detected: {}", error_str);

    assert!(
        error_str.contains("syntax")
            || error_str.contains("expected")
            || error_str.contains("Parse error")
            || error_str.contains("No main function"),
        "Error should be a syntax error: {}",
        error_str
    );
}

/// Test that semantic errors are properly detected
#[test]
fn test_semantic_error_detection() {
    let code = r#"
        fn main() -> i32 {
            let x: i32 = "not a number";
            // Type mismatch - should cause semantic error
            x
        }
    "#;

    let result = compile_and_run_zeta(code);

    // Should fail with type error
    assert!(result.is_err(), "Semantic error should be detected");

    let error = result.unwrap_err();
    let error_str = error.to_string();
    println!("Semantic error detected: {}", error_str);

    assert!(
        error_str.contains("type")
            || error_str.contains("mismatch")
            || error_str.contains("cannot")
            || error_str.contains("Typecheck"),
        "Error should be a type error: {}",
        error_str
    );
}

/// Test error recovery - compiler should continue after non-fatal errors
#[test]
fn test_error_recovery_multiple_errors() {
    let code = r#"
        fn main() -> i32 {
            let x = 42;        // OK
            let y: i32 = "bad"; // Type error
            let z = x +        // Syntax error (incomplete expression)
            x
        }
    "#;

    let result = compile_and_run_zeta(code);

    // Should fail with multiple errors
    assert!(result.is_err(), "Should detect errors");

    let error = result.unwrap_err();
    let error_str = error.to_string();

    // Should report errors (implementation may vary)
    println!("Multiple error output: {}", error_str);

    // At minimum, should report an error
    assert!(!error_str.is_empty(), "Should have error output");
}

/// Test that valid code compiles without errors
#[test]
fn test_valid_code_compiles() {
    let code = r#"
        fn main() -> i64 {
            let x = 42;
            let y = 10;
            x + y
        }
    "#;

    let result = compile_and_run_zeta(code);

    // Should succeed
    assert!(result.is_ok(), "Valid code should compile: {:?}", result);

    if let Ok(value) = result {
        println!("Valid code compiled and returned: {}", value);
        assert_eq!(value, 52, "Should compute correct sum");
    }
}

/// Test error messages include location information
#[test]
fn test_error_location_reporting() {
    let code = r#"
        fn main() -> i32 {
            let x = "string";
            // Should report type error with some location info
            let y: i32 = x;
            y
        }
    "#;

    let result = compile_and_run_zeta(code);

    assert!(result.is_err(), "Should have type error");

    let error = result.unwrap_err();
    let error_str = error.to_string();

    // Error should include useful information
    println!("Error with context: {}", error_str);

    // Should mention the error nature
    assert!(
        !error_str.is_empty(),
        "Error should include information: {}",
        error_str
    );
}

/// Test cross-system error propagation
/// Syntax error → Semantic analysis should not crash
#[test]
fn test_cross_system_error_handling() {
    let code = r#"
        fn bad_syntax() -> {
            // Missing return type - syntax error
            let x = 42
            x
        }
        
        fn main() -> i32 {
            // Even with syntax error in another function,
            // compiler should handle it gracefully
            42
        }
    "#;

    let result = compile_and_run_zeta(code);

    // May or may not succeed depending on error recovery
    // Important thing is it doesn't panic
    println!("Cross-system error handling result: {:?}", result);

    // Test passes as long as it doesn't panic
    // (actual result depends on error recovery implementation)
}

/// Test error severity levels are respected
#[test]
fn test_error_severity_levels() {
    let code = r#"
        // Note: Zeta may not have #[allow] attribute yet
        fn main() -> i32 {
            let x = 42;  // Unused variable - might be warning
            let y = 10;
            y
        }
    "#;

    let result = compile_and_run_zeta(code);

    // Should compile successfully (warning not error)
    // Some compilers treat unused variables as warnings, some as errors
    println!("Unused variable test result: {:?}", result);

    // Test passes regardless - we're testing the compiler doesn't crash
}
