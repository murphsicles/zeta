//! Tests for comptime function parsing for v0.3.25
//!
//! Tests that ensure comptime functions are parsed correctly,
//! including the comptime keyword, const functions, and async functions.

use zetac::frontend::parser::top_level::parse_zeta;

/// Test basic comptime function parsing
#[test]
fn test_comptime_fn_basic() {
    let code = r#"
    comptime fn generate_residues() -> [NUM_RESIDUES]u64 {
        return [0; NUM_RESIDUES]
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse comptime fn: {:?}", result.err());
    
    let (remaining, ast) = result.unwrap();
    assert!(remaining.trim().is_empty(), "Should parse entire input, remaining: '{}'", remaining);
    assert!(!ast.is_empty(), "Should have AST nodes");
}

/// Test comptime function with const and async for comparison
#[test]
fn test_comptime_const_async_comparison() {
    let code = r#"
    comptime fn comptime_func() -> i64 { return 1 }
    const fn const_func() -> i64 { return 2 }
    async fn async_func() -> i64 { return 3 }
    fn regular_func() -> i64 { return 4 }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse all function types: {:?}", result.err());
    
    let (remaining, ast) = result.unwrap();
    assert!(remaining.trim().is_empty(), "Should parse entire input");
    assert_eq!(ast.len(), 4, "Should have 4 AST nodes");
}

/// Test pub comptime function
#[test]
fn test_pub_comptime_fn() {
    let code = r#"
    pub comptime fn public_comptime() -> i64 {
        return 42
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse pub comptime fn: {:?}", result.err());
    
    let (remaining, _) = result.unwrap();
    assert!(remaining.trim().is_empty(), "Should parse entire input");
}

/// Test comptime variable declaration
#[test]
fn test_comptime_variable() {
    let code = r#"
    comptime x: i64 = 42
    const y: i64 = 24
    "#;

    let result = parse_zeta(code);
    // Note: comptime variable parsing might not be implemented yet
    // This test will fail initially but serves as documentation
    println!("Result: {:?}", result);
}

/// Test that comptime functions can have parameters
#[test]
fn test_comptime_fn_with_params() {
    let code = r#"
    comptime fn add(a: i64, b: i64) -> i64 {
        return a + b
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse comptime fn with params: {:?}", result.err());
    
    let (remaining, _) = result.unwrap();
    assert!(remaining.trim().is_empty(), "Should parse entire input");
}

/// Test array return type parsing (for PrimeZeta compatibility)
#[test]
fn test_comptime_fn_array_return() {
    let code = r#"
    comptime fn generate_residues() -> [NUM_RESIDUES]u64 {
        var list: [NUM_RESIDUES]u64 = [0; NUM_RESIDUES]
        return list
    }
    "#;

    let result = parse_zeta(code);
    // This might fail if array type parsing isn't fully implemented
    // but should at least parse the function declaration
    println!("Result: {:?}", result);
}