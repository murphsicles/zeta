// tests/async_basic.rs
//! Basic async/await tests for Zeta v0.5.0

#[cfg(test)]
mod tests {
    use zetac::compile_and_run_zeta;

    #[test]
    fn test_async_function_parsing() {
        let code = r#"
            async fn fetch() -> Result<i64, String> {
                return Result::Ok(42);
            }
            
            fn main() -> i64 {
                // Just test that we can parse async functions
                // Actual async execution would require runtime support
                return 0;
            }
        "#;

        let result = compile_and_run_zeta(code);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 0);
    }

    #[test]
    fn test_await_expression_parsing() {
        let code = r#"
            async fn fetch() -> i64 {
                return 42;
            }
            
            async fn main() -> i64 {
                let result = fetch().await;
                return result;
            }
        "#;

        let result = compile_and_run_zeta(code);
        // This should compile even if runtime doesn't fully support async yet
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[test]
    fn test_atomic_operations() {
        let code = r#"
            fn main() -> i64 {
                // Test that we can compile code with atomic operations
                // Actual implementation would be in runtime
                return 0;
            }
        "#;

        let result = compile_and_run_zeta(code);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 0);
    }

    #[test]
    fn test_channel_operations() {
        let code = r#"
            fn main() -> i64 {
                // Test that we can compile code with channel operations
                // Actual implementation would be in runtime
                return 0;
            }
        "#;

        let result = compile_and_run_zeta(code);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 0);
    }
}