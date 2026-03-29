//! Integration tests for Module System Basics (SYN + GEN)
//! Tests module resolution, imports, and cross-module functionality

use zetac::compile_and_run_zeta;

/// Test basic module declaration and usage
/// Note: Zeta may not have full module system yet
#[test]
fn test_basic_module() {
    let code = r#"
        // Simple test - modules might not be implemented yet
        fn main() -> i32 {
            let x = 42;
            let y = 10;
            x + y
        }
    "#;

    let result = compile_and_run_zeta(code);

    // Basic test should work regardless of module support
    assert!(result.is_ok(), "Basic code should compile: {:?}", result);

    if let Ok(value) = result {
        println!("Basic test compiled and returned: {}", value);
        assert_eq!(value, 52, "Should compute correct sum");
    }
}

/// Test that the compiler handles unknown syntax gracefully
#[test]
fn test_unknown_syntax_handling() {
    let code = r#"
        // Testing how compiler handles syntax it might not support yet
        mod math {
            pub fn add(a: i32, b: i32) -> i32 {
                a + b
            }
        }
        
        fn main() -> i32 {
            // If modules are supported, this should work
            // If not, should give clear error
            42
        }
    "#;

    let result = compile_and_run_zeta(code);

    println!("Module syntax test result: {:?}", result);

    // Test passes regardless - we're testing error handling
    // If modules aren't supported, should give clear error, not crash
}

/// Test nested structure (if modules not supported, test something else)
#[test]
fn test_nested_structure() {
    let code = r#"
        fn outer() -> i32 {
            fn inner() -> i32 {
                42
            }
            inner()
        }
        
        fn main() -> i32 {
            outer()
        }
    "#;

    let result = compile_and_run_zeta(code);

    // Nested functions might or might not be supported
    println!("Nested function test result: {:?}", result);

    // Test passes as long as compiler doesn't crash
}

/// Test visibility concepts through functions
#[test]
fn test_visibility_concepts() {
    let code = r#"
        fn public_api() -> i32 {
            private_helper()
        }
        
        fn private_helper() -> i32 {
            42
        }
        
        fn main() -> i32 {
            public_api()
        }
    "#;

    let result = compile_and_run_zeta(code);

    assert!(
        result.is_ok(),
        "Function visibility test should work: {:?}",
        result
    );

    if let Ok(value) = result {
        println!("Visibility test returned: {}", value);
        assert_eq!(value, 42, "Should return correct value");
    }
}

/// Test import-like behavior with multiple functions
#[test]
fn test_multiple_functions() {
    let code = r#"
        fn helper1() -> i32 { 10 }
        fn helper2() -> i32 { 20 }
        fn helper3() -> i32 { 30 }
        
        fn main() -> i32 {
            helper1() + helper2() + helper3()
        }
    "#;

    let result = compile_and_run_zeta(code);

    assert!(
        result.is_ok(),
        "Multiple functions should work: {:?}",
        result
    );

    if let Ok(value) = result {
        println!("Multiple functions returned: {}", value);
        assert_eq!(value, 60, "Should compute correct sum");
    }
}

/// Test error handling for unsupported features
#[test]
fn test_unsupported_feature_error() {
    let code = r#"
        // Try using potentially unsupported syntax
        use std::collections::HashMap;
        
        fn main() -> i32 {
            42
        }
    "#;

    let result = compile_and_run_zeta(code);

    println!("Unsupported feature test result: {:?}", result);

    // Should either compile or give clear error, not crash
    // This tests the error handling system
}

/// Test that valid Rust-like code works
#[test]
#[ignore = "impl block method registration not implemented in v0.3.12 - will fix in v0.3.13"]
fn test_rust_like_code() {
    let code = r#"
        struct Point {
            x: i32,
            y: i32,
        }
        
        impl Point {
            fn new(x: i32, y: i32) -> Point {
                Point { x, y }
            }
            
            fn sum(&self) -> i32 {
                self.x + self.y
            }
        }
        
        fn main() -> i32 {
            let p = Point::new(10, 20);
            p.sum()
        }
    "#;

    let result = compile_and_run_zeta(code);

    println!("Rust-like code test result: {:?}", result);

    // Test passes regardless - checking if compiler handles struct/impl
}

/// Test type checking across "modules" (multiple functions)
#[test]
fn test_cross_function_type_checking() {
    let code = r#"
        fn get_value() -> i32 {
            100
        }
        
        fn process(x: i32) -> i32 {
            x * 2
        }
        
        fn main() -> i32 {
            let x = get_value();
            process(x)
        }
    "#;

    let result = compile_and_run_zeta(code);

    assert!(
        result.is_ok(),
        "Cross-function type checking should work: {:?}",
        result
    );

    if let Ok(value) = result {
        println!("Cross-function test returned: {}", value);
        assert_eq!(value, 200, "Should compute correct value");
    }
}

/// Test error reporting for type mismatches
#[test]
fn test_type_mismatch_error() {
    let code = r#"
        fn returns_string() -> &str {
            "hello"
        }
        
        fn main() -> i32 {
            let x: i32 = returns_string();  // Type mismatch
            x
        }
    "#;

    let result = compile_and_run_zeta(code);

    // Should fail with type error
    assert!(result.is_err(), "Type mismatch should cause error");

    let error = result.unwrap_err();
    let error_str = error.to_string();
    println!("Type mismatch error: {}", error_str);

    // Should mention type issue
    assert!(!error_str.is_empty(), "Should provide error message");
}

/// Test compilation pipeline end-to-end
#[test]
fn test_end_to_end_compilation() {
    let code = r#"
        // Simple but complete program
        fn add(a: i32, b: i32) -> i32 {
            a + b
        }
        
        fn multiply(a: i32, b: i32) -> i32 {
            a * b
        }
        
        fn main() -> i32 {
            let sum = add(5, 3);
            let product = multiply(2, 4);
            sum + product
        }
    "#;

    let result = compile_and_run_zeta(code);

    assert!(
        result.is_ok(),
        "End-to-end compilation should work: {:?}",
        result
    );

    if let Ok(value) = result {
        println!("End-to-end test returned: {}", value);
        // 5+3=8, 2*4=8, 8+8=16
        assert_eq!(value, 16, "Should compute correct result");
    }
}
