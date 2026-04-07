#[test]
fn test_bool_return_type() {
    use zetac::frontend::parser::top_level::parse_zeta;
    
    let test_code = r#"
fn test_bool() -> bool {
    return true
}

fn main() -> u64 {
    return 42
}
"#;
    
    let result = parse_zeta(test_code);
    assert!(result.is_ok(), "Failed to parse bool return type: {:?}", result);
    
    let (remaining, ast) = result.unwrap();
    assert!(remaining.trim().is_empty(), "Didn't parse all input: '{}'", remaining);
    assert_eq!(ast.len(), 2, "Expected 2 AST nodes, got {}", ast.len());
    
    println!("✅ Bool return type parsing works!");
}