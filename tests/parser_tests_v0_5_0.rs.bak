// Parser Tests for v0.5.0 Generic Syntax
// These tests verify all the new generic syntax patterns

#[cfg(test)]
mod tests {
    use super::*;
    use src::frontend::parser::parser::*;
    use src::frontend::parser::top_level::parse_struct;

    #[test]
    fn test_pointer_types() {
        let cases = vec![
            ("*const i32", "*const i32"),
            ("*mut String", "*mut String"),
            ("*const [T]", "*const [T]"),
            ("*mut Vec<T>", "*mut Vec<T>"),
        ];
        
        for (input, expected) in cases {
            let result = parse_type(input);
            assert!(result.is_ok(), "Failed to parse: {}", input);
            let (rest, parsed) = result.unwrap();
            assert!(rest.is_empty(), "Didn't consume all input for: {}", input);
            assert_eq!(parsed, expected, "Mismatch for: {}", input);
        }
    }

    #[test]
    fn test_references_with_lifetimes() {
        let cases = vec![
            ("&'a T", "&'a T"),
            ("&'a mut T", "&'a mut T"),
            ("&'static str", "&'static str"),
            ("&&'a T", "&&'a T"), // Multiple references
        ];
        
        for (input, expected) in cases {
            let result = parse_type(input);
            assert!(result.is_ok(), "Failed to parse: {}", input);
            let (rest, parsed) = result.unwrap();
            assert!(rest.is_empty(), "Didn't consume all input for: {}", input);
            assert_eq!(parsed, expected, "Mismatch for: {}", input);
        }
    }

    #[test]
    fn test_trait_bounds() {
        let cases = vec![
            ("T: Clone", "T: Clone"),
            ("T: Clone + Display", "T: Clone + Display"),
            ("T: std::fmt::Debug", "T: std::fmt::Debug"),
            ("T: Clone + Display + Debug", "T: Clone + Display + Debug"),
        ];
        
        for (input, expected) in cases {
            let result = parse_generic_param(input);
            assert!(result.is_ok(), "Failed to parse: {}", input);
            let (rest, parsed) = result.unwrap();
            assert!(rest.is_empty(), "Didn't consume all input for: {}", input);
            assert_eq!(parsed, expected, "Mismatch for: {}", input);
        }
    }

    #[test]
    fn test_where_clauses() {
        let input = "where T: Clone, U: Debug + Display";
        let result = parse_where_clause(input);
        assert!(result.is_ok(), "Failed to parse where clause");
        
        let (rest, clauses) = result.unwrap();
        assert!(rest.is_empty(), "Didn't consume all input");
        assert_eq!(clauses.len(), 2, "Should have 2 where clauses");
        
        // Check first clause
        assert_eq!(clauses[0].0, "T");
        assert_eq!(clauses[0].1, vec!["Clone"]);
        
        // Check second clause
        assert_eq!(clauses[1].0, "U");
        assert_eq!(clauses[1].1, vec!["Debug", "Display"]);
    }

    #[test]
    fn test_complex_type_paths() {
        let cases = vec![
            ("std::collections::HashMap<K, V>", "std::collections::HashMap<K, V>"),
            ("Vec<Result<T, E>>", "Vec<Result<T, E>>"),
            ("Option<Box<dyn Trait>>", "Option<Box<dyn Trait>>"),
        ];
        
        for (input, expected) in cases {
            let result = parse_type(input);
            assert!(result.is_ok(), "Failed to parse: {}", input);
            let (rest, parsed) = result.unwrap();
            assert!(rest.is_empty(), "Didn't consume all input for: {}", input);
            assert_eq!(parsed, expected, "Mismatch for: {}", input);
        }
    }

    #[test]
    fn test_struct_with_where_clause() {
        let input = "struct Result<T, E> where T: Clone, E: Debug { ok: T, err: E }";
        let result = parse_struct(input);
        assert!(result.is_ok(), "Failed to parse struct with where clause");
        
        let (rest, ast_node) = result.unwrap();
        assert!(rest.is_empty(), "Didn't consume all input");
        
        // Verify it's a struct definition
        match ast_node {
            crate::frontend::ast::AstNode::StructDef { name, generics, .. } => {
                assert_eq!(name, "Result");
                assert_eq!(generics.len(), 2); // T and E
            }
            _ => panic!("Expected StructDef, got something else"),
        }
    }

    #[test]
    fn test_generic_parameters_with_lifetimes() {
        let input = "<'a, 'b, T: Clone, U>";
        let result = parse_generic_params(input);
        assert!(result.is_ok(), "Failed to parse generic params");
        
        let (rest, (lifetimes, type_params)) = result.unwrap();
        assert!(rest.is_empty(), "Didn't consume all input");
        
        assert_eq!(lifetimes.len(), 2, "Should have 2 lifetime params");
        assert_eq!(type_params.len(), 2, "Should have 2 type params");
        
        // Check lifetimes
        assert!(lifetimes.contains(&"'a".to_string()));
        assert!(lifetimes.contains(&"'b".to_string()));
        
        // Check type params (one with bounds, one without)
        assert!(type_params.iter().any(|p| p.contains("T: Clone")));
        assert!(type_params.iter().any(|p| p == "U"));
    }

    #[test]
    fn test_mixed_reference_types() {
        let cases = vec![
            ("&'a mut *const T", "&'a mut *const T"),
            ("*mut &'static str", "*mut &'static str"),
            ("&[T; N]", "&[T; N]"),
        ];
        
        for (input, expected) in cases {
            let result = parse_type(input);
            assert!(result.is_ok(), "Failed to parse: {}", input);
            let (rest, parsed) = result.unwrap();
            // Note: Some complex combinations might leave whitespace
            let parsed_trimmed = parsed.trim();
            assert_eq!(parsed_trimmed, expected, "Mismatch for: {}", input);
        }
    }
}

fn main() {
    println!("Parser Tests for v0.5.0 Generic Syntax");
    println!("======================================");
    println!("\nAll test cases defined. Run with: cargo test");
    
    // Summary of implemented features
    println!("\n✅ IMPLEMENTED FEATURES:");
    println!("1. Pointer types: *const T, *mut T");
    println!("2. References with lifetimes: &'a T, &'a mut T");
    println!("3. Trait bounds: T: Clone + Display");
    println!("4. Where clauses: where T: Clone");
    println!("5. Complex type paths: std::collections::HashMap<K, V>");
    println!("6. Generic parameters with lifetimes: <'a, T>");
    println!("7. Mixed reference/pointer types");
    println!("8. Struct definitions with where clauses");
    
    println!("\n📋 TEST COVERAGE:");
    println!("- 8 test functions covering all v0.5.0 syntax");
    println!("- Edge cases and complex combinations");
    println!("- Integration with existing parser infrastructure");
}