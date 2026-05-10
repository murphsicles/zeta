//! Test utilities for static method support
//! Provides helper functions for testing static method compilation

use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::new_resolver::type_check;
// Note: compile_zeta function might be in a different module
// For now, we'll just parse and type check

/// Test helper for parsing static method calls
pub fn parse_static_method_call(code: &str) -> Result<(), String> {
    let result = parse_zeta(code);
    match result {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                return Err(format!("Unparsed input: {}", remaining));
            }
            if ast.is_empty() {
                return Err("Empty AST".to_string());
            }
            Ok(())
        }
        Err(e) => Err(format!("Parse error: {:?}", e)),
    }
}

/// Test helper for type checking static method calls
pub fn type_check_static_method(code: &str) -> Result<(), String> {
    let parse_result = parse_zeta(code);
    match parse_result {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                return Err(format!("Unparsed input: {}", remaining));
            }
            match type_check(&ast) {
                Ok(_) => Ok(()),
                Err(e) => Err(format!("Type check error: {:?}", e)),
            }
        }
        Err(e) => Err(format!("Parse error: {:?}", e)),
    }
}

/// Test helper for full compilation of static method code
pub fn compile_static_method(code: &str) -> Result<(), String> {
    let parse_result = parse_zeta(code);
    match parse_result {
        Ok((remaining, ast)) => {
            if !remaining.is_empty() {
                return Err(format!("Unparsed input: {}", remaining));
            }
            // For now, just type check since compile_zeta might not be available
            match type_check(&ast) {
                Ok(_) => Ok(()),
                Err(e) => Err(format!(
                    "Type check error (as proxy for compilation): {:?}",
                    e
                )),
            }
        }
        Err(e) => Err(format!("Parse error: {:?}", e)),
    }
}

/// Expected output validator for static method tests
#[allow(dead_code)]
pub fn validate_expected_output(_code: &str, _expected_output: &str) -> Result<(), String> {
    // This would run the code and compare output
    // For now, just a placeholder
    Ok(())
}

/// Error message tester for static method edge cases
#[allow(dead_code)]
pub fn test_error_message(code: &str, expected_error_pattern: &str) -> Result<(), String> {
    let result = parse_zeta(code);
    match result {
        Ok((remaining, _)) => {
            if remaining.is_empty() {
                Err("Expected parse error but got success".to_string())
            } else {
                Err(format!(
                    "Expected parse error but got partial parse with remaining: {}",
                    remaining
                ))
            }
        }
        Err(e) => {
            let error_str = format!("{:?}", e);
            if error_str.contains(expected_error_pattern) {
                Ok(())
            } else {
                Err(format!(
                    "Error '{}' doesn't contain pattern '{}'",
                    error_str, expected_error_pattern
                ))
            }
        }
    }
}

/// Test matrix generator for static method variations
pub struct StaticMethodTestMatrix {
    pub simple_static: Vec<&'static str>,
    pub generic_static: Vec<&'static str>,
    #[allow(dead_code)]
    pub impl_block_static: Vec<&'static str>,
    #[allow(dead_code)]
    pub associated_functions: Vec<&'static str>,
    #[allow(dead_code)]
    pub edge_cases: Vec<&'static str>,
    pub error_cases: Vec<(&'static str, &'static str)>, // (code, expected_error_pattern)
}

impl Default for StaticMethodTestMatrix {
    fn default() -> Self {
        Self::new()
    }
}

impl StaticMethodTestMatrix {
    pub fn new() -> Self {
        StaticMethodTestMatrix {
            simple_static: vec![
                "Point::new(10, 20)",
                "Vec::new()",
                "Option::None",
                "Result::Ok(42)",
            ],
            generic_static: vec![
                "Vec::<i32>::new()",
                "Option::<bool>::None",
                "HashMap::<String, i32>::new()",
                "Result::<i32, String>::Ok(42)",
            ],
            impl_block_static: vec![
                "impl Point { fn new(x: i64, y: i64) -> Point { Point { x, y } } }",
                "impl<T> Vec<T> { fn new() -> Vec<T> { Vec { data: [] } } }",
            ],
            associated_functions: vec![
                "Point::default()",
                "String::from(\"hello\")",
                "Vec::with_capacity(10)",
            ],
            edge_cases: vec![
                "A::B::C::method()",         // Nested path
                "Self::new()",               // Self keyword
                "super::module::function()", // Super keyword
                "crate::module::function()", // Crate-relative path
            ],
            error_cases: vec![
                ("Point::", "Expected method name after ::"),
                ("::method()", "Expected path before ::"),
                ("Point::new(10,", "Unclosed parentheses"),
                ("Vec::<i32>", "Expected :: after type arguments"),
            ],
        }
    }
}
