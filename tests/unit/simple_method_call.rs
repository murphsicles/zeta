//! Simple test for method call parsing

#[cfg(test)]
mod tests {
    use zetac::frontend::parser::expr::parse_expr;

    #[test]
    fn test_simple_method_call() {
        let code = "foo.bar()";
        let result = parse_expr(code);
        println!("Result: {:?}", result);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, _ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
    }

    #[test]
    fn test_method_call_with_args() {
        let code = "foo.bar(1, 2)";
        let result = parse_expr(code);
        println!("Result: {:?}", result);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, _ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
    }

    #[test]
    fn test_field_access() {
        let code = "foo.bar";
        let result = parse_expr(code);
        println!("Result: {:?}", result);
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());

        let (remaining, _ast) = result.unwrap();
        assert!(remaining.is_empty(), "Unparsed input: {}", remaining);
    }
}
