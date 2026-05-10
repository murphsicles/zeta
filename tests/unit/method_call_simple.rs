//! Simple tests for method call syntax

#[cfg(test)]
mod tests {
    use zetac::frontend::parser::top_level::parse_zeta;

    #[test]
    fn test_simple_function() {
        let code = r#"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        
        fn main() {
            let result = add(1, 2);
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
        assert_eq!(ast.len(), 2, "Expected 2 AST nodes, got {}", ast.len());
    }

    #[test]
    fn test_function_with_self() {
        let code = r#"
        fn point_add(self: i64, other: i64) -> i64 {
            self + other
        }
        
        fn main() {
            let result = point_add(1, 2);
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
        assert_eq!(ast.len(), 2, "Expected 2 AST nodes, got {}", ast.len());
    }

    #[test]
    fn test_struct_and_method() {
        let code = r#"
        struct Point {
            x: i64,
            y: i64,
        }
        
        fn main() {
            let p = Point { x: 1, y: 2 };
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
        assert_eq!(ast.len(), 2, "Expected 2 AST nodes, got {}", ast.len());
    }
}
