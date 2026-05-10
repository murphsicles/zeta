//! Tests for method call parsing (just parsing, not type checking)

#[cfg(test)]
mod tests {
    use zetac::frontend::parser::top_level::parse_zeta;

    #[test]
    fn test_method_call_parsing() {
        let code = r#"
        fn main() {
            let x = foo.bar();
            let y = foo.bar(1, 2);
            let z = foo.bar::<i32>(1, 2);
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
        assert_eq!(ast.len(), 1, "Expected 1 AST node, got {}", ast.len());
    }

    #[test]
    fn test_chained_method_calls_parsing() {
        let code = r#"
        fn main() {
            let x = foo.bar().baz().qux();
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
        assert_eq!(ast.len(), 1, "Expected 1 AST node, got {}", ast.len());
    }

    #[test]
    fn test_field_access_parsing() {
        let code = r#"
        fn main() {
            let x = foo.bar;
            let y = foo.bar.baz;
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
        assert_eq!(ast.len(), 1, "Expected 1 AST node, got {}", ast.len());
    }

    #[test]
    fn test_mixed_method_and_field_access() {
        let code = r#"
        fn main() {
            let x = foo.bar().baz.qux().quux;
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
        assert_eq!(ast.len(), 1, "Expected 1 AST node, got {}", ast.len());
    }
}
