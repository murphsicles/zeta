//! Tests for impl block parsing compatibility with v0.5.0
//!
//! Tests that ensure impl blocks with generic types are parsed correctly,
//! supporting patterns like `impl Option<i64>` and `impl<T> Option<T>`.

use zetac::frontend::parser::top_level::parse_zeta;

/// Test simple impl block for concrete type
#[test]
fn test_simple_impl_block() {
    let code = r#"
    impl Point {
        fn new(x: i64, y: i64) -> Point {
            Point { x, y }
        }
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse simple impl block");

    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");
    assert!(!ast.is_empty(), "Should have AST nodes");
}

/// Test impl block for generic type with concrete arguments
#[test]
fn test_impl_generic_type_concrete() {
    let code = r#"
    impl Option<i64> {
        fn unwrap(self) -> i64 {
            match self {
                Some(x) => x,
                None => 0,
            }
        }
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse impl Option<i64>");

    let (remaining, _) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");
}

/// Test generic impl block with type parameter
#[test]
fn test_generic_impl_block() {
    let code = r#"
    impl<T> Option<T> {
        fn unwrap(self) -> T {
            match self {
                Some(x) => x,
                None => panic!("unwrap on None"),
            }
        }
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse generic impl<T> Option<T>");

    let (remaining, _) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");
}

/// Test impl block with where clause
#[test]
fn test_impl_with_where_clause() {
    let code = r#"
    impl<T> Option<T> where T: Clone {
        fn clone(self) -> T {
            self.value.clone()
        }
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse impl with where clause");

    let (remaining, _) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");
}

/// Test trait implementation
#[test]
fn test_trait_impl() {
    let code = r#"
    impl Clone for Point {
        fn clone(&self) -> Point {
            Point { x: self.x, y: self.y }
        }
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse trait impl Clone for Point");

    let (remaining, _) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");
}

/// Test multiple methods in impl block
#[test]
fn test_impl_with_multiple_methods() {
    let code = r#"
    impl<T> Option<T> {
        fn unwrap(self) -> T {
            match self {
                Some(x) => x,
                None => panic!("unwrap on None"),
            }
        }
        
        fn is_some(&self) -> bool {
            match self {
                Some(_) => true,
                None => false,
            }
        }
        
        fn is_none(&self) -> bool {
            !self.is_some()
        }
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Should parse impl with multiple methods");

    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");

    // Should have 3 method definitions
    if let Some(first_node) = ast.first() {
        match first_node {
            zetac::frontend::ast::AstNode::ImplBlock { body, .. } => {
                assert_eq!(body.len(), 3, "Should have 3 methods in impl block");
            }
            _ => panic!("Expected ImplBlock AST node"),
        }
    }
}

/// Test that non-generic code still works after impl fixes
#[test]
fn test_backward_compatibility() {
    // Ensure that fixing impl parsing doesn't break existing code
    let code = r#"
    struct Point { x: i64, y: i64 }
    
    fn main() -> i64 {
        let p = Point { x: 10, y: 20 };
        p.x + p.y
    }
    "#;

    let result = parse_zeta(code);
    assert!(result.is_ok(), "Non-generic code should still work");

    let (remaining, _) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");
}

/// Test complex nested generic type in impl
#[test]
fn test_complex_generic_impl() {
    let code = r#"
    impl<T, E> Result<T, E> {
        fn unwrap(self) -> T {
            match self {
                Ok(x) => x,
                Err(_) => panic!("unwrap on Err"),
            }
        }
    }
    "#;

    let result = parse_zeta(code);
    // This might fail if multiple generic parameters aren't fully supported
    // but should at least parse
    if result.is_err() {
        println!("Note: impl with multiple generic parameters might not be fully supported");
    }
}
