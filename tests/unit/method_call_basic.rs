//! Basic tests for method call syntax
//! Tests that method calls parse correctly for both struct instances and module functions

#[cfg(test)]
mod tests {
    use zetac::frontend::parser::top_level::parse_zeta;
    use zetac::middle::resolver::new_resolver::type_check;

    #[test]
    fn test_basic_method_call() {
        let code = r#"
        struct Point {
            x: i64,
            y: i64,
        }
        
        fn point_new(x: i64, y: i64) -> Point {
            Point { x, y }
        }
        
        fn point_add(self: Point, other: Point) -> Point {
            Point { x: self.x + other.x, y: self.y + other.y }
        }
        
        fn main() {
            let p1 = point_new(1, 2);
            let p2 = point_new(3, 4);
            let p3 = p1.add(p2);
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

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
    fn test_module_function_call() {
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

        // Type check should pass
        let type_check_result = type_check(&ast);
        assert!(
            type_check_result.is_ok(),
            "Type check failed: {:?}",
            type_check_result.err()
        );
    }

    #[test]
    fn test_chained_method_calls() {
        let code = r#"
        struct Point {
            x: i64,
            y: i64,
        }
        
        fn point_new(x: i64, y: i64) -> Point {
            Point { x, y }
        }
        
        fn point_add(self: Point, other: Point) -> Point {
            Point { x: self.x + other.x, y: self.y + other.y }
        }
        
        fn point_scale(self: Point, factor: i64) -> Point {
            Point { x: self.x * factor, y: self.y * factor }
        }
        
        fn main() {
            let p1 = point_new(1, 2);
            let p2 = point_new(3, 4);
            let p3 = p1.add(p2).scale(2);
        }
        "#;

        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

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
}
