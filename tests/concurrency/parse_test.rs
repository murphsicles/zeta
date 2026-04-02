// tests/concurrency/parse_test.rs
// Test that async/await syntax parses correctly

#[test]
fn test_async_function_parsing() {
    let code = r#"
async fn get_value() -> i32 {
    42
}

fn main() -> i32 {
    get_value().await
}
"#;
    
    let result = zetac::parse_zeta(code);
    assert!(result.is_ok(), "Failed to parse async function: {:?}", result);
    
    let (remaining, asts) = result.unwrap();
    assert!(remaining.trim().is_empty(), "Incomplete parse: '{}'", remaining);
    assert!(!asts.is_empty(), "No AST nodes generated");
    
    println!("Successfully parsed async function with {} AST nodes", asts.len());
}

#[test]
fn test_async_with_await_parsing() {
    let code = r#"
async fn add(x: i32, y: i32) -> i32 {
    x + y
}

async fn compute() -> i32 {
    let a = add(1, 2).await;
    let b = add(3, 4).await;
    a + b
}
"#;
    
    let result = zetac::parse_zeta(code);
    assert!(result.is_ok(), "Failed to parse async with await: {:?}", result);
    
    let (remaining, asts) = result.unwrap();
    assert!(remaining.trim().is_empty(), "Incomplete parse: '{}'", remaining);
    
    println!("Successfully parsed async with await expressions");
}