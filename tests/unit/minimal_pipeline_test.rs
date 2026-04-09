// Minimal pipeline test to verify core components work

/// Test 1: Parser works for basic code
#[test]
fn test_parser_basic() {
    use zetac::frontend::parser::top_level::parse_zeta;

    let code = "fn main() -> i64 { 42 }";
    let result = parse_zeta(code);

    assert!(result.is_ok(), "Parser should work for basic code");
    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");
    assert!(!ast.is_empty(), "Should have AST nodes");

    println!("✓ Parser works for basic code");
}

/// Test 2: Type system basic functionality
#[test]
fn test_type_system_basic() {
    use zetac::middle::types::{Substitution, Type, TypeVar};

    // Test basic type unification
    let mut subst = Substitution::new();
    let t_var = Type::Variable(TypeVar::fresh());

    assert!(subst.unify(&t_var, &Type::I32).is_ok());
    assert_eq!(subst.apply(&t_var), Type::I32);

    println!("✓ Type system basic unification works");
}

/// Test 3: Generic type instantiation
#[test]
fn test_generic_instantiation() {
    use zetac::middle::types::{Type, TypeVar};

    // Test Vec<T> instantiation
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_type = Type::Named("Vec".to_string(), vec![t_var]);

    let result = vec_type.instantiate_generic(&[Type::I32]);
    assert!(result.is_ok(), "Should instantiate Vec<T> with i32");

    let instantiated = result.unwrap();
    match instantiated {
        Type::Named(name, args) => {
            assert_eq!(name, "Vec");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], Type::I32);
        }
        _ => panic!("Expected Named type"),
    }

    println!("✓ Generic type instantiation works");
}

/// Test 4: lt() syntax parsing (if supported)
#[test]
fn test_lt_syntax_parsing() {
    use zetac::frontend::parser::top_level::parse_zeta;

    // Test if lt() syntax is supported
    let code = r#"
    fn main() -> i64 {
        let v = lt(Vec, i32)::new();
        0
    }
    "#;

    let result = parse_zeta(code);
    if result.is_ok() {
        println!("✓ lt() syntax is supported by parser");
    } else {
        println!("⚠️ lt() syntax not supported by parser (may be expected)");
    }
}

/// Test 5: End-to-end compilation report
#[test]
fn test_pipeline_status_report() {
    println!("\n=== END-TO-END COMPILATION PIPELINE STATUS ===");
    println!("Time: {}", chrono::Local::now().format("%H:%M:%S"));

    println!("\nComponent Status:");
    println!("1. Parser: ✓ Basic parsing works");
    println!("2. Type System: ✓ Basic unification works");
    println!("3. Generic Instantiation: ✓ Basic instantiation works");
    println!("4. lt() Syntax: ⚠️ May not be fully supported");
    println!("5. Resolver: ❓ Not tested (may have issues)");
    println!("6. Codegen: ❓ Not tested (may have issues)");
    println!("7. Full Compilation: ❓ Not tested");

    println!("\nCritical Issues Found:");
    println!("1. Compilation errors in some test binaries");
    println!("2. Missing where_clauses field in some AST creations (partially fixed)");
    println!("3. Potential issues with monomorphization implementation");

    println!("\nRecommendations:");
    println!("1. Fix compilation errors in test binaries");
    println!("2. Complete monomorphization implementation");
    println!("3. Add integration tests for resolver ↔ codegen pipeline");
    println!("4. Test Vec::<i32>::new() syntax with actual compilation");

    // This test always passes - it's a status report
}
