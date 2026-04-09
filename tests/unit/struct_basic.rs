//! Basic struct tests for Zeta v0.3.13
//! Tests struct definition, instantiation, and field access

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::new_resolver::type_check;

#[test]
fn test_struct_definition() {
    let code = r#"
        struct Point {
            x: i32,
            y: i32,
        }
        
        fn main() -> i64 {
            0
        }
    "#;

    let result = parse_zeta(code);
    assert!(
        result.is_ok(),
        "Failed to parse struct definition: {:?}",
        result.err()
    );

    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Unparsed input: {}", remaining);

    // Type check should pass
    let type_check_result = type_check(&ast);
    assert!(
        type_check_result.is_ok(),
        "Type check failed: {:?}",
        type_check_result.err()
    );
}

#[test]
fn test_struct_instantiation() {
    let code = r#"
        struct Point {
            x: i32,
            y: i32,
        }
        
        fn main() -> i64 {
            let p = Point { x: 10, y: 20 };
            0
        }
    "#;

    let result = parse_zeta(code);
    assert!(
        result.is_ok(),
        "Failed to parse struct instantiation: {:?}",
        result.err()
    );

    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Unparsed input: {}", remaining);

    // Type check should pass
    let type_check_result = type_check(&ast);
    assert!(
        type_check_result.is_ok(),
        "Type check failed: {:?}",
        type_check_result.err()
    );
}

#[test]
fn test_struct_field_access() {
    let code = r#"
        struct Point {
            x: i32,
            y: i32,
        }
        
        fn main() -> i64 {
            let p = Point { x: 10, y: 20 };
            p.x + p.y
        }
    "#;

    let result = parse_zeta(code);
    assert!(
        result.is_ok(),
        "Failed to parse struct field access: {:?}",
        result.err()
    );

    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Unparsed input: {}", remaining);

    // Type check should pass
    let type_check_result = type_check(&ast);
    assert!(
        type_check_result.is_ok(),
        "Type check failed: {:?}",
        type_check_result.err()
    );
}

#[test]
fn test_nested_struct() {
    let code = r#"
        struct Point {
            x: i32,
            y: i32,
        }
        
        struct Line {
            start: Point,
            end: Point,
        }
        
        fn main() -> i64 {
            let p1 = Point { x: 0, y: 0 };
            let p2 = Point { x: 10, y: 20 };
            let line = Line { start: p1, end: p2 };
            line.start.x + line.end.y
        }
    "#;

    let result = parse_zeta(code);
    assert!(
        result.is_ok(),
        "Failed to parse nested struct: {:?}",
        result.err()
    );

    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Unparsed input: {}", remaining);

    // Type check should pass
    let type_check_result = type_check(&ast);
    assert!(
        type_check_result.is_ok(),
        "Type check failed: {:?}",
        type_check_result.err()
    );
}
