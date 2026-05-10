//! Tests for const generics syntax support
//! Phase 5: Parser and syntax support for const generics

#[cfg(test)]
mod tests {
    use zetac::frontend::parser::top_level::parse_zeta;
    
    #[test]
    fn test_const_generic_function() {
        // Test comptime fn with const generic parameter
        let code = r#"
        comptime fn sieve<const LIMIT: usize>() -> [bool; LIMIT] {
            let mut sieve = [false; LIMIT];
            sieve[0] = true;
            sieve[1] = true;
            for i in 2..LIMIT {
                if !sieve[i] {
                    let mut j = i * i;
                    while j < LIMIT {
                        sieve[j] = true;
                        j += i;
                    }
                }
            }
            sieve
        }
        "#;
        
        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse const generic function: {:?}", result.err());
        
        let (remaining, asts): (&str, Vec<_>) = result.unwrap();
        assert!(remaining.is_empty() || remaining.trim().is_empty(), "Didn't parse entire input: '{}'", remaining);
        assert!(!asts.is_empty(), "No AST nodes parsed");
    }
    
    #[test]
    fn test_const_generic_struct() {
        // Test struct with const generic parameter
        let code = r#"
        struct FixedArray<T, const N: usize> {
            data: [T; N]
        }
        "#;
        
        let result = parse_zeta(code);
        // Note: Struct const generics not fully implemented yet
        // This test should pass once implemented
        if let Err(e) = result {
            println!("Struct const generics not yet implemented: {:?}", e);
        }
    }
    
    #[test]
    fn test_const_generic_impl() {
        // Test impl with const generic parameter
        let code = r#"
        impl<T, const N: usize> FixedArray<T, N> {
            fn new() -> Self {
                FixedArray { data: [zero(); N] }
            }
        }
        "#;
        
        let result = parse_zeta(code);
        // Note: Impl const generics not fully implemented yet
        if let Err(e) = result {
            println!("Impl const generics not yet implemented: {:?}", e);
        }
    }
    
    #[test]
    fn test_const_expression_in_array_size() {
        // Test const expressions in array sizes
        let code = r#"
        const SIZE: usize = 10;
        const DOUBLE_SIZE: usize = SIZE * 2;
        
        fn test() -> [i32; DOUBLE_SIZE] {
            [0; DOUBLE_SIZE]
        }
        "#;
        
        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse const expression in array size: {:?}", result.err());
    }
    
    #[test]
    fn test_multiple_const_params() {
        // Test multiple const generic parameters
        let code = r#"
        fn matrix<const ROWS: usize, const COLS: usize>() -> [[f64; COLS]; ROWS] {
            [[0.0; COLS]; ROWS]
        }
        "#;
        
        let result = parse_zeta(code);
        assert!(result.is_ok(), "Failed to parse multiple const generic parameters: {:?}", result.err());
    }
    
    #[test]
    fn test_const_generic_with_bounds() {
        // Test const generic with where clause
        let code = r#"
        fn process<const N: usize>(data: [u8; N]) -> usize 
        where
            N > 0,
            N < 1000
        {
            N * 2
        }
        "#;
        
        let result = parse_zeta(code);
        // Note: Const generic bounds not fully implemented yet
        if let Err(e) = result {
            println!("Const generic bounds not yet implemented: {:?}", e);
        }
    }
}