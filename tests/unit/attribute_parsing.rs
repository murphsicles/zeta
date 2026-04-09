//! Tests for attribute parsing
//!
//! Tests that attributes like #[derive(Copy)] are parsed correctly

use zetac::frontend::parser::top_level::parse_zeta;

#[test]
fn test_attribute_parsing_on_function() {
    let source = r#"
        #[inline]
        fn add(x: i64, y: i64) -> i64 {
            x + y
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::FuncDef { name, attrs, .. } => {
            assert_eq!(name, "add");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "inline");
        }
        _ => panic!("Expected FuncDef, got {:?}", asts[0]),
    }
}

#[test]
fn test_attribute_parsing_multiple_attributes() {
    let source = r#"
        #[inline]
        #[cold]
        fn compute() -> i64 {
            42
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::FuncDef { name, attrs, .. } => {
            assert_eq!(name, "compute");
            assert_eq!(attrs.len(), 2);
            assert_eq!(attrs[0], "inline");
            assert_eq!(attrs[1], "cold");
        }
        _ => panic!("Expected FuncDef, got {:?}", asts[0]),
    }
}

#[test]
fn test_attribute_parsing_with_arguments() {
    let source = r#"
        #[derive(Copy, Clone)]
        struct Point {
            x: i64,
            y: i64,
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::StructDef { name, attrs, .. } => {
            assert_eq!(name, "Point");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "derive(Copy, Clone)");
        }
        _ => panic!("Expected StructDef, got {:?}", asts[0]),
    }
}

#[test]
fn test_attribute_parsing_on_enum() {
    let source = r#"
        #[repr(u8)]
        enum Status {
            Ok,
            Error,
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::EnumDef { name, attrs, .. } => {
            assert_eq!(name, "Status");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "repr(u8)");
        }
        _ => panic!("Expected EnumDef, got {:?}", asts[0]),
    }
}

#[test]
fn test_attribute_parsing_on_impl() {
    let source = r#"
        #[allow(unused)]
        impl Point {
            fn new(x: i64, y: i64) -> Point {
                Point { x, y }
            }
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::ImplBlock { attrs, .. } => {
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "allow(unused)");
        }
        _ => panic!("Expected ImplBlock, got {:?}", asts[0]),
    }
}

#[test]
fn test_attribute_parsing_on_concept() {
    let source = r#"
        #[marker]
        concept Copy {
            fn copy(&self) -> Self;
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::ConceptDef { name, attrs, .. } => {
            assert_eq!(name, "Copy");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "marker");
        }
        _ => panic!("Expected ConceptDef, got {:?}", asts[0]),
    }
}

#[test]
fn test_attribute_parsing_on_method() {
    let source = r#"
        concept Display {
            #[must_use]
            fn display(&self) -> String;
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::ConceptDef { methods, .. } => {
            assert_eq!(methods.len(), 1);
            match &methods[0] {
                zetac::frontend::ast::AstNode::Method { name, attrs, .. } => {
                    assert_eq!(name, "display");
                    assert_eq!(attrs.len(), 1);
                    assert_eq!(attrs[0], "must_use");
                }
                _ => panic!("Expected Method in concept"),
            }
        }
        _ => panic!("Expected ConceptDef, got {:?}", asts[0]),
    }
}

#[test]
fn test_attribute_parsing_complex_attribute() {
    let source = r#"
        #[cfg(target_os = "linux")]
        #[feature(specialization)]
        fn linux_only() -> i64 {
            0
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::FuncDef { name, attrs, .. } => {
            assert_eq!(name, "linux_only");
            assert_eq!(attrs.len(), 2);
            assert_eq!(attrs[0], "cfg(target_os = \"linux\")");
            assert_eq!(attrs[1], "feature(specialization)");
        }
        _ => panic!("Expected FuncDef, got {:?}", asts[0]),
    }
}
