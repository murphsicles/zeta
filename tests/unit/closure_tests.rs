//! Tests for closure functionality in Zeta v0.3.18

use zetac::middle::resolver::new_resolver::type_check;
use zetac::parse_zeta;

#[test]
fn test_basic_closure() {
    let code = r#"
        let x = 5;
        let add_five = |y| y + x;
        let result = add_five(10);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    assert!(result.is_ok(), "Type checking failed: {:?}", result);
}

#[test]
fn test_closure_with_multiple_params() {
    let code = r#"
        let x = 5;
        let y = 10;
        let add = |a, b| a + b + x + y;
        let result = add(1, 2);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    assert!(result.is_ok(), "Type checking failed: {:?}", result);
}

#[test]
fn test_closure_capturing_multiple_vars() {
    let code = r#"
        let x = 5;
        let y = 10;
        let z = 15;
        let sum = || x + y + z;
        let result = sum();
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    assert!(result.is_ok(), "Type checking failed: {:?}", result);
}

#[test]
fn test_closure_in_expression() {
    let code = r#"
        let x = 5;
        let result = (|y| y + x)(10);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    assert!(result.is_ok(), "Type checking failed: {:?}", result);
}

#[test]
fn test_nested_closures() {
    let code = r#"
        let x = 5;
        let outer = |y| {
            let inner = |z| z + y + x;
            inner(10)
        };
        let result = outer(20);
    "#;

    let (_, ast) = parse_zeta(code).expect("Failed to parse");
    let result = type_check(&ast);
    assert!(result.is_ok(), "Type checking failed: {:?}", result);
}
