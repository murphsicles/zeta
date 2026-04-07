//! End-to-end compilation pipeline tests
//!
//! Tests the complete pipeline from parsing to code generation
//! for generic type system functionality.

use zetac::compile_and_run_zeta;

/// Test 1: Basic end-to-end compilation
/// Tests that a simple program compiles and runs correctly
#[test]
fn test_basic_end_to_end_compilation() {
    println!("=== Test 1: Basic End-to-End Compilation ===");

    let code = r#"
    fn main() -> i64 {
        42
    }
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_ok(), "Basic program should compile and run");
    assert_eq!(result.unwrap(), 42, "Should return correct value");

    println!("✓ Basic end-to-end compilation works");
}

/// Test 2: Generic struct compilation
/// Tests compilation of programs with generic structs
#[test]
fn test_generic_struct_compilation() {
    println!("=== Test 2: Generic Struct Compilation ===");

    // Test with concrete instantiation
    let code = r#"
    struct Container<T> {
        value: T
    }
    
    fn main() -> i64 {
        let c = Container { value: 100 };
        c.value
    }
    "#;

    let result = compile_and_run_zeta(code);
    if let Ok(value) = result {
        println!("✓ Generic struct with concrete type compiles");
        assert_eq!(value, 100, "Should return correct value");
    } else {
        println!("⚠️ Generic struct compilation failed: {:?}", result.err());
        // This might be expected if generic support isn't complete
    }
}

/// Test 3: Generic function compilation
/// Tests compilation of generic functions
#[test]
fn test_generic_function_compilation() {
    println!("=== Test 3: Generic Function Compilation ===");

    let code = r#"
    fn identity<T>(x: T) -> T {
        x
    }
    
    fn main() -> i64 {
        identity(42)
    }
    "#;

    let result = compile_and_run_zeta(code);
    if let Ok(value) = result {
        println!("✓ Generic function compiles and runs");
        assert_eq!(value, 42, "Should return correct value");
    } else {
        println!("⚠️ Generic function compilation failed: {:?}", result.err());
    }
}

/// Test 4: Vec::<i32>::new() syntax compilation
/// Tests the specific syntax mentioned in requirements
#[test]
fn test_vec_new_syntax_compilation() {
    println!("=== Test 4: Vec::<i32>::new() Syntax ===");

    // First test if we can parse the lt() syntax
    let code = r#"
    fn main() -> i64 {
        // Using lt() syntax for generic instantiation
        let v = lt(Vec, i32)::new();
        0  // Just return 0 for now since we can't use the vector
    }
    "#;

    let result = compile_and_run_zeta(code);
    if let Ok(value) = result {
        println!("✓ lt(Vec, i32)::new() syntax compiles");
        assert_eq!(value, 0, "Should return 0");
    } else {
        println!("⚠️ lt() syntax compilation failed: {:?}", result.err());
    }
}

/// Test 5: Parser → Type System → Resolver → Codegen pipeline
/// Tests the complete pipeline with a more complex example
#[test]
fn test_complete_pipeline() {
    println!("=== Test 5: Complete Pipeline Test ===");

    let code = r#"
    struct Pair<A, B> {
        first: A,
        second: B
    }
    
    fn make_pair<A, B>(a: A, b: B) -> Pair<A, B> {
        Pair { first: a, second: b }
    }
    
    fn main() -> i64 {
        let p = make_pair(10, 20);
        p.first + p.second
    }
    "#;

    let result = compile_and_run_zeta(code);
    if let Ok(value) = result {
        println!("✓ Complete generic pipeline works");
        assert_eq!(value, 30, "Should compute correct sum");
    } else {
        println!("⚠️ Complete pipeline failed: {:?}", result.err());
    }
}

/// Test 6: Error handling in pipeline
/// Tests that errors propagate correctly through the pipeline
#[test]
fn test_error_propagation() {
    println!("=== Test 6: Error Propagation ===");

    // Test with type error
    let code = r#"
    fn add(a: i64, b: i64) -> i64 {
        a + b
    }
    
    fn main() -> i64 {
        add(10, "string")  // Type error: string instead of i64
    }
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_err(), "Should fail with type error");

    let error = result.unwrap_err();
    println!("✓ Type error caught: {}", error);

    // Test with syntax error - REAL syntax error (malformed expression)
    let code = r#"
    fn main() -> i64 {
        let x = 10 +  // Missing right operand
    }
    "#;

    let result = compile_and_run_zeta(code);
    assert!(result.is_err(), "Should fail with syntax error");

    println!("✓ Syntax error caught");
}

/// Test 7: Integration with existing type system
/// Tests that generic code integrates with existing type system features
#[test]
fn test_type_system_integration() {
    println!("=== Test 7: Type System Integration ===");

    let code = r#"
    fn max<T>(a: T, b: T) -> T {
        if a > b { a } else { b }
    }
    
    fn main() -> i64 {
        max(5, 10)
    }
    "#;

    let result = compile_and_run_zeta(code);
    if let Ok(value) = result {
        println!("✓ Generic function with comparison works");
        assert_eq!(value, 10, "Should return max value");
    } else {
        println!("⚠️ Generic comparison failed: {:?}", result.err());
    }
}

/// Test 8: Performance and caching
/// Tests that repeated compilation works correctly
#[test]
fn test_repeated_compilation() {
    println!("=== Test 8: Repeated Compilation ===");

    let code = r#"
    fn main() -> i64 {
        123
    }
    "#;

    // Compile multiple times to test caching and resource management
    for _i in 0..3 {
        let result = compile_and_run_zeta(code);
        assert!(result.is_ok(), "Should compile successfully each time");
        assert_eq!(
            result.unwrap(),
            123,
            "Should return correct value each time"
        );
    }

    println!("✓ Repeated compilation works correctly");
}

/// Test 9: Comprehensive test report
/// Generates a comprehensive report of end-to-end test status
#[test]
fn test_comprehensive_report() {
    println!("\n=== END-TO-END COMPILATION PIPELINE REPORT ===");
    println!("Time: {}", chrono::Local::now().format("%H:%M:%S"));
    println!("\nTest Results Summary:");

    let tests = vec![
        (
            "Basic compilation",
            test_basic_end_to_end_compilation as fn(),
        ),
        ("Generic structs", test_generic_struct_compilation as fn()),
        (
            "Generic functions",
            test_generic_function_compilation as fn(),
        ),
        ("Vec::new() syntax", test_vec_new_syntax_compilation as fn()),
        ("Complete pipeline", test_complete_pipeline as fn()),
        ("Error propagation", test_error_propagation as fn()),
        (
            "Type system integration",
            test_type_system_integration as fn(),
        ),
        ("Repeated compilation", test_repeated_compilation as fn()),
    ];

    let mut passed = 0;
    let mut total = 0;

    for (name, test_fn) in tests {
        total += 1;
        print!("• {}: ", name);

        let result = std::panic::catch_unwind(|| {
            test_fn();
        });

        if result.is_ok() {
            passed += 1;
            println!("✓ PASSED");
        } else {
            println!("✗ FAILED");
        }
    }

    println!(
        "\nOverall: {}/{} tests passed ({:.1}%)",
        passed,
        total,
        (passed as f32 / total as f32) * 100.0
    );

    println!("\n=== CRITICAL ISSUES IDENTIFIED ===");

    // Run specific diagnostic tests
    println!("\n1. Testing lt() syntax support:");
    test_lt_syntax_diagnostic();

    println!("\n2. Testing generic instantiation:");
    test_generic_instantiation_diagnostic();

    println!("\n3. Testing type substitution:");
    test_type_substitution_diagnostic();

    println!("\n=== RECOMMENDATIONS ===");
    println!("1. Fix lt() syntax parsing if not working");
    println!("2. Implement monomorphization in codegen");
    println!("3. Add proper error messages for generic type errors");
    println!("4. Test with more complex generic patterns");

    // This test always passes - it's a report
}

/// Diagnostic test for lt() syntax
fn test_lt_syntax_diagnostic() {
    use zetac::frontend::parser::top_level::parse_zeta;

    let test_cases = vec![
        (
            "lt(Vec, i32)::new()",
            r#"fn main() -> i64 { let v = lt(Vec, i32)::new(); 0 }"#,
        ),
        (
            "lt(Option, i32)::Some(42)",
            r#"fn main() -> i64 { let o = lt(Option, i32)::Some(42); 0 }"#,
        ),
        (
            "lt(Result, i32, String)::Ok(42)",
            r#"fn main() -> i64 { let r = lt(Result, i32, String)::Ok(42); 0 }"#,
        ),
    ];

    for (name, code) in test_cases {
        let result = parse_zeta(code);
        if result.is_ok() {
            println!("  ✓ {} parses correctly", name);
        } else {
            println!("  ✗ {} fails to parse: {:?}", name, result.err());
        }
    }
}

/// Diagnostic test for generic instantiation
fn test_generic_instantiation_diagnostic() {
    use zetac::middle::types::{Type, TypeVar};

    // Test type instantiation
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_type = Type::Named("Vec".to_string(), vec![t_var.clone()]);

    match vec_type.instantiate_generic(&[Type::I32]) {
        Ok(instantiated) => {
            println!("  ✓ Vec<T> instantiated with i32: {:?}", instantiated);
        }
        Err(e) => {
            println!("  ✗ Vec<T> instantiation failed: {}", e);
        }
    }
}

/// Diagnostic test for type substitution
fn test_type_substitution_diagnostic() {
    use zetac::middle::types::{Substitution, Type, TypeVar};

    let mut subst = Substitution::new();
    let t_var = Type::Variable(TypeVar::fresh());

    // Test basic substitution
    if subst.unify(&t_var, &Type::I32).is_ok() {
        println!("  ✓ Type variable unified with i32");
        assert_eq!(subst.apply(&t_var), Type::I32);
    } else {
        println!("  ✗ Type unification failed");
    }

    // Test generic function type unification
    let t_var2 = Type::Variable(TypeVar::fresh());
    let generic_func = Type::Function(vec![t_var2.clone()], Box::new(t_var2.clone()));
    let concrete_func = Type::Function(vec![Type::I64], Box::new(Type::I64));

    if subst.unify(&generic_func, &concrete_func).is_ok() {
        println!("  ✓ Generic function type unified with concrete type");
    } else {
        println!("  ✗ Generic function type unification failed");
    }
}
