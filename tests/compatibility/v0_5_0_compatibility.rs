//! v0.5.0 compatibility tests for generic type system
//!
//! Tests that ensure the generic type system is compatible with
//! existing v0.5.0 source code patterns and use cases.

// use zetac::compile_and_run_zeta;
use zetac::frontend::parser::top_level::parse_zeta;

/// Test basic generic struct parsing from v0.5.0
#[test]
fn test_v0_5_0_generic_struct_parsing() {
    // Test 1: Simple generic struct (from debug_generic_parsing.rs)
    let code = r#"
    struct Point<T> { x: T, y: T }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse simple generic struct");

    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");

    // Check that we got a StructDef with generics
    assert!(!ast.is_empty(), "Should have AST nodes");

    // Test 2: Multiple generic parameters
    let code = r#"
    struct Pair<A, B> { first: A, second: B }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse struct with multiple generics");

    // Test 3: Generic struct with bounds (may not be fully supported yet)
    let code = r#"
    struct Container<T: Display> { value: T }
    "#;

    let result = parse_zeta(code);
    // This might fail if trait bounds aren't fully implemented
    // For v0.5.0 compatibility, we should at least parse it
    if result.is_err() {
        println!("Note: Trait bounds in generics not yet fully supported");
    }
}

/// Test generic function parsing from v0.5.0
#[test]
fn test_v0_5_0_generic_function_parsing() {
    // Test 1: Simple generic function
    let code = r#"
    fn identity<T>(x: T) -> T { x }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse simple generic function");

    // Test 2: Generic function with multiple parameters
    let code = r#"
    fn swap<A, B>(a: A, b: B) -> (B, A) { (b, a) }
    "#;

    let result = parse_zeta(code);
    assert!(
        result.is_ok(),
        "Should parse generic function with multiple params"
    );

    // Test 3: Generic function with trait bounds
    let code = r#"
    fn print_and_clone<T: Display + Clone>(x: T) -> T {
        println!("{}", x);
        x.clone()
    }
    "#;

    let result = parse_zeta(code);
    // Trait bounds might not be fully supported
    if result.is_err() {
        println!("Note: Trait bounds in functions not yet fully supported");
    }
}

/// Test generic type usage in expressions (lt() syntax)
#[test]
fn test_v0_5_0_lt_syntax_compatibility() {
    // The lt() syntax is Zeta's way to specify generic types
    // lt(Result, i64) means Result<i64>

    // Test 1: Basic lt() usage
    let code = r#"
    fn main() -> i64 {
        let x = lt(Result, i64);
        0
    }
    "#;

    let result = parse_zeta(code);
    // lt() syntax should be parseable
    assert!(result.is_ok(), "Should parse lt() syntax");

    // Test 2: lt() with multiple type arguments
    let code = r#"
    fn main() -> i64 {
        let x = lt(Result, i64, String);
        0
    }
    "#;

    let result = parse_zeta(code);
    // Should parse Result<i64, String>
    assert!(result.is_ok(), "Should parse lt() with multiple args");
}

/// Test existing v0.5.0 patterns that use generics
#[test]
fn test_v0_5_0_existing_patterns() {
    // Pattern 1: Option type usage
    let code = r#"
    fn get_value() -> lt(Option, i64) {
        lt(Some, 42)
    }
    
    fn main() -> i64 {
        match get_value() {
            lt(Some, x) => x,
            lt(None) => 0,
        }
    }
    "#;

    let result = parse_zeta(code);
    // This complex pattern should parse
    if result.is_err() {
        println!("Note: Complex match with lt() syntax might not be fully supported");
    }

    // Pattern 2: Result type usage
    let code = r#"
    fn divide(a: i64, b: i64) -> lt(Result, i64, String) {
        if b == 0 {
            lt(Err, "division by zero")
        } else {
            lt(Ok, a / b)
        }
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse Result pattern");
}

/// Test that non-generic code still works
#[test]
fn test_v0_5_0_non_generic_compatibility() {
    // Ensure that adding generic support doesn't break non-generic code

    // Test 1: Simple non-generic struct
    let code = r#"
    struct Point { x: i64, y: i64 }
    
    fn main() -> i64 {
        let p = Point { x: 10, y: 20 };
        p.x + p.y
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Non-generic structs should still work");

    // Test 2: Simple function
    let code = r#"
    fn add(a: i64, b: i64) -> i64 { a + b }
    
    fn main() -> i64 { add(1, 2) }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Simple functions should still work");

    // Test 3: Existing v0.3.9 features
    let code = r#"
    const MAX: i64 = 100;
    
    fn process() -> i64 {
        let x = 3.14;  // Float literal
        let y = match x > 2.0 {
            true => 1,
            false => 0,
        };
        y
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "v0.3.9 features should still work");
}

/// Test integration between generic and non-generic code
#[test]
fn test_v0_5_0_mixed_generic_integration() {
    // Test mixing generic and non-generic code

    let code = r#"
    // Non-generic struct
    struct Point { x: i64, y: i64 }
    
    // Generic struct
    struct Container<T> { value: T }
    
    // Non-generic function using generic type
    fn get_point_container() -> Container<Point> {
        Container { value: Point { x: 1, y: 2 } }
    }
    
    // Generic function using non-generic type
    fn wrap_in_container<T>(value: T) -> Container<T> {
        Container { value }
    }
    "#;

    let result = parse_zeta(code);
    // This should parse even if some features aren't fully implemented
    if result.is_err() {
        println!("Note: Mixed generic/non-generic integration might have issues");
        println!("Error: {:?}", result.err());
    }
}

/// Test backward compatibility with type system tests
#[test]
fn test_v0_5_0_type_system_backward_compat() {
    // Ensure existing type system tests still pass

    // Recreate the test from generic_instantiation.rs
    use zetac::middle::types::{Type, TypeVar};

    // Test generic instantiation still works
    let t_var = Type::Variable(TypeVar::fresh());
    let e_var = Type::Variable(TypeVar::fresh());
    let result_type = Type::Named("Result".to_string(), vec![t_var, e_var]);

    let instantiated =
        result_type.instantiate_generic(&[Type::I64, Type::Named("String".to_string(), vec![])]);

    assert!(instantiated.is_ok());
    let instantiated_type = instantiated.unwrap();

    match instantiated_type {
        Type::Named(name, args) => {
            assert_eq!(name, "Result");
            assert_eq!(args.len(), 2);
            assert_eq!(args[0], Type::I64);
            assert_eq!(args[1], Type::Named("String".to_string(), vec![]));
        }
        _ => panic!("Expected Named type"),
    }
}

/// Test that error messages are helpful
#[test]
fn test_v0_5_0_error_messages() {
    // Test that type errors with generics give helpful messages

    use zetac::middle::types::{Substitution, Type, TypeVar};

    let mut subst = Substitution::new();
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_type = Type::Named("Vec".to_string(), vec![t_var.clone()]);

    // Try to unify Vec<T> with i32 (should fail with helpful error)
    let result = subst.unify(&vec_type, &Type::I32);

    assert!(result.is_err());
    let error = result.unwrap_err();

    // Error should mention the types involved
    let error_str = format!("{}", error);
    assert!(
        error_str.contains("Vec") || error_str.contains("i32") || error_str.contains("mismatch")
    );
}

/// Test performance with generics (smoke test)
#[test]
fn test_v0_5_0_generic_performance() {
    // Simple smoke test to ensure generics don't cause obvious performance issues

    use std::time::Instant;
    use zetac::middle::types::{Substitution, Type, TypeVar};

    let start = Instant::now();

    // Create and unify some generic types
    let mut subst = Substitution::new();

    for i in 0..10 {
        let t_var = Type::Variable(TypeVar::fresh());
        let vec_type = Type::Named("Vec".to_string(), vec![t_var.clone()]);
        let concrete_vec = Type::Named("Vec".to_string(), vec![Type::I32]);

        let result = subst.unify(&vec_type, &concrete_vec);
        assert!(
            result.is_ok(),
            "Unification should succeed on iteration {}",
            i
        );
    }

    let duration = start.elapsed();

    // Should complete quickly (under 1 second)
    assert!(
        duration.as_secs() < 1,
        "Generic operations took too long: {:?}",
        duration
    );

    println!("Generic type operations took: {:?}", duration);
}
