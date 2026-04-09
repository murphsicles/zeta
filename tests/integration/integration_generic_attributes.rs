//! Integration test for generic instantiation with attributes
//!
//! Tests that we can parse code with attributes and generic types

use zetac::frontend::parser::top_level::parse_zeta;

#[test]
fn test_generic_struct_with_attributes() {
    let source = r#"
        #[derive(Copy, Clone)]
        struct Point<T> {
            x: T,
            y: T,
        }
        
        fn main() -> i64 {
            let p: Point<i64> = Point { x: 1, y: 2 };
            0
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 2);

    // Check struct definition
    match &asts[0] {
        zetac::frontend::ast::AstNode::StructDef { name, attrs, .. } => {
            assert_eq!(name, "Point");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "derive(Copy, Clone)");
        }
        _ => panic!("Expected StructDef, got {:?}", asts[0]),
    }

    // Check function
    match &asts[1] {
        zetac::frontend::ast::AstNode::FuncDef { name, .. } => {
            assert_eq!(name, "main");
        }
        _ => panic!("Expected FuncDef, got {:?}", asts[1]),
    }
}

#[test]
fn test_generic_enum_with_attributes() {
    let source = r#"
        #[repr(u8)]
        enum Result<T, E> {
            Ok(T),
            Err(E),
        }
        
        #[inline]
        fn get_ok() -> Result<i64, String> {
            Result::Ok(42)
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 2);

    // Check enum definition
    match &asts[0] {
        zetac::frontend::ast::AstNode::EnumDef { name, attrs, .. } => {
            assert_eq!(name, "Result");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "repr(u8)");
        }
        _ => panic!("Expected EnumDef, got {:?}", asts[0]),
    }

    // Check function with attribute
    match &asts[1] {
        zetac::frontend::ast::AstNode::FuncDef { name, attrs, .. } => {
            assert_eq!(name, "get_ok");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "inline");
        }
        _ => panic!("Expected FuncDef, got {:?}", asts[1]),
    }
}

#[test]
fn test_concept_with_attributes() {
    let source = r#"
        #[marker]
        concept Copy {
            #[must_use]
            fn copy(&self) -> Self;
        }
        
        #[derive(Copy)]
        struct MyType {
            value: i64,
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 2);

    // Check concept definition
    match &asts[0] {
        zetac::frontend::ast::AstNode::ConceptDef {
            name,
            attrs,
            methods,
            ..
        } => {
            assert_eq!(name, "Copy");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "marker");

            // Check method attribute
            assert_eq!(methods.len(), 1);
            match &methods[0] {
                zetac::frontend::ast::AstNode::Method { name, attrs, .. } => {
                    assert_eq!(name, "copy");
                    assert_eq!(attrs.len(), 1);
                    assert_eq!(attrs[0], "must_use");
                }
                _ => panic!("Expected Method in concept"),
            }
        }
        _ => panic!("Expected ConceptDef, got {:?}", asts[0]),
    }

    // Check struct with derive attribute
    match &asts[1] {
        zetac::frontend::ast::AstNode::StructDef { name, attrs, .. } => {
            assert_eq!(name, "MyType");
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "derive(Copy)");
        }
        _ => panic!("Expected StructDef, got {:?}", asts[1]),
    }
}

#[test]
fn test_impl_with_attributes() {
    let source = r#"
        #[allow(unused_variables)]
        impl Point {
            #[inline]
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

    // Check impl block with attribute
    match &asts[0] {
        zetac::frontend::ast::AstNode::ImplBlock { attrs, body, .. } => {
            assert_eq!(attrs.len(), 1);
            assert_eq!(attrs[0], "allow(unused_variables)");

            // Check method in impl with attribute
            assert_eq!(body.len(), 1);
            match &body[0] {
                zetac::frontend::ast::AstNode::FuncDef { name, attrs, .. } => {
                    assert_eq!(name, "new");
                    assert_eq!(attrs.len(), 1);
                    assert_eq!(attrs[0], "inline");
                }
                _ => panic!("Expected FuncDef in impl body"),
            }
        }
        _ => panic!("Expected ImplBlock, got {:?}", asts[0]),
    }
}

#[test]
fn test_complex_attribute_syntax() {
    let source = r#"
        #[cfg(target_os = "linux")]
        #[feature(specialization)]
        fn linux_only<T>(value: T) -> T {
            value
        }
    "#;

    let result = parse_zeta(source);
    assert!(result.is_ok());

    let (remaining, asts) = result.unwrap();
    assert!(remaining.is_empty());
    assert_eq!(asts.len(), 1);

    match &asts[0] {
        zetac::frontend::ast::AstNode::FuncDef {
            name,
            attrs,
            generics,
            ..
        } => {
            assert_eq!(name, "linux_only");
            assert_eq!(attrs.len(), 2);
            assert_eq!(attrs[0], "cfg(target_os = \"linux\")");
            assert_eq!(attrs[1], "feature(specialization)");
            // Check that generics are parsed
            assert_eq!(generics.len(), 1);
            assert_eq!(generics[0], "T");
        }
        _ => panic!("Expected FuncDef, got {:?}", asts[0]),
    }
}
