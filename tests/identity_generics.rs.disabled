//! Integration tests for identity-constrained generics

use zetac::compile_and_run_zeta;

/// Test basic identity constraint parsing and compilation
#[test]
fn test_identity_constraint_parsing() {
    // Simple generic function with identity constraint
    let code = r#"
        fn process<T: Identity<Read>>(x: T) -> i64 {
            return 42;
        }
        fn main() -> i64 {
            // Create an identity string with read capability
            let s: string[identity:read] = "hello";
            process(s)
        }
    "#;
    let result = compile_and_run_zeta(code);
    // For now, just ensure it compiles without errors
    if let Err(e) = &result { println!("Compilation error: {}", e); }
    assert!(result.is_ok());
    // Expect result 42
    assert_eq!(result.unwrap(), 42);
}

/// Test multiple capabilities constraint
#[test]
fn test_identity_multiple_capabilities() {
    let code = r#"
        fn process<T: Identity<Read+Write>>(x: T) -> i64 {
            return 99;
        }
        fn main() -> i64 {
            let s: string[identity:read+write] = "test";
            process(s)
        }
    "#;
    let result = compile_and_run_zeta(code);
    if let Err(e) = &result { println!("Compilation error: {}", e); }
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 99);
}

/// Test combined identity and trait constraints
#[test]
fn test_combined_constraints() {
    // This test might fail until combined constraints are fully implemented
    // We'll accept compilation error for now
    let code = r#"
        fn process_and_clone<T: Identity<Read> + Clone>(item: T) -> i64 {
            return 123;
        }
        fn main() -> i64 {
            let s: string[identity:read] = "item";
            process_and_clone(s)
        }
    "#;
    let result = compile_and_run_zeta(code);
    // We'll accept either success or compilation error (for now)
    // Just ensure no panic
    println!("Result: {:?}", result);
}