// Test for dereferencing operations
use zetac::frontend::borrow::BorrowChecker;
use zetac::frontend::parser::expr::parse_full_expr;
use zetac::middle::resolver::resolver::{Resolver, Type};
use zetac::middle::types::Mutability;
use zetac::middle::types::lifetime::Lifetime;

#[test]
fn test_deref_parsing() {
    println!("=== Testing Deref Parsing ===");

    // Test 1: Simple dereference
    let code = "*x";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse dereference: {:?}", result);
    println!("✓ Parsed '*x' successfully");

    // Test 2: Dereference with spaces
    let code = "* x";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '* x': {:?}", result);
    println!("✓ Parsed '* x' successfully");

    // Test 3: Multiple dereferences
    let code = "**x";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '**x': {:?}", result);
    println!("✓ Parsed '**x' successfully");

    // Test 4: Dereference in expression
    let code = "*x + 1";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '*x + 1': {:?}", result);
    println!("✓ Parsed '*x + 1' successfully");

    // Test 5: Reference and dereference
    let code = "&*x";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '&*x': {:?}", result);
    println!("✓ Parsed '&*x' successfully");

    println!("=== All deref parsing tests passed ===");
}

#[test]
fn test_reference_creation_parsing() {
    println!("\n=== Testing Reference Creation Parsing ===");

    // Test 1: Immutable reference
    let code = "&x";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '&x': {:?}", result);
    println!("✓ Parsed '&x' successfully");

    // Test 2: Mutable reference
    let code = "&mut x";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '&mut x': {:?}", result);
    println!("✓ Parsed '&mut x' successfully");

    // Test 3: Reference to dereference
    let code = "&*x";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '&*x': {:?}", result);
    println!("✓ Parsed '&*x' successfully");

    // Test 4: Mutable reference to field
    let code = "&mut x.field";
    let result = parse_full_expr(code);
    assert!(
        result.is_ok(),
        "Failed to parse '&mut x.field': {:?}",
        result
    );
    println!("✓ Parsed '&mut x.field' successfully");

    println!("=== All reference creation parsing tests passed ===");
}

#[test]
fn test_borrow_checker_with_deref() {
    println!("\n=== Testing Borrow Checker with Deref ===");

    let mut checker = BorrowChecker::new();
    let resolver = Resolver::new();

    // Declare a variable as borrowed (like a reference)
    checker.declare(
        "x".to_string(),
        zetac::frontend::borrow::BorrowState::Borrowed,
        Type::Ref(Box::new(Type::I32), Lifetime::Static, Mutability::Immutable),
    );

    // Parse a dereference expression
    let code = "*x";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse: {:?}", result);

    let (_, ast) = result.unwrap();
    // Check that the borrow checker allows dereferencing
    assert!(checker.check(&ast, &resolver));
    println!("✓ Borrow checker allows dereferencing borrowed variable");

    println!("=== All borrow checker with deref tests passed ===");
}

#[test]
fn test_complex_reference_expressions() {
    println!("\n=== Testing Complex Reference Expressions ===");

    // Test 1: Dereference in assignment
    let code = "*x = 42";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '*x = 42': {:?}", result);
    println!("✓ Parsed '*x = 42' successfully");

    // Test 2: Reference in function call
    let code = "f(&x)";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse 'f(&x)': {:?}", result);
    println!("✓ Parsed 'f(&x)' successfully");

    // Test 3: Mutable reference in function call
    let code = "f(&mut x)";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse 'f(&mut x)': {:?}", result);
    println!("✓ Parsed 'f(&mut x)' successfully");

    // Test 4: Dereference in binary operation
    let code = "*x + *y";
    let result = parse_full_expr(code);
    assert!(result.is_ok(), "Failed to parse '*x + *y': {:?}", result);
    println!("✓ Parsed '*x + *y' successfully");

    println!("=== All complex reference expression tests passed ===");
}

fn main() {
    println!("Running deref tests...");
    test_deref_parsing();
    test_reference_creation_parsing();
    test_borrow_checker_with_deref();
    test_complex_reference_expressions();
    println!("\n=== All deref tests completed successfully ===");
}
