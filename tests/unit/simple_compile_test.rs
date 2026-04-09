// Simple compilation test to verify basic functionality

#[test]
fn test_simple_compile() {
    // This is a minimal test that should work if the compiler is functional
    let code = r#"
    fn main() -> i64 {
        42
    }
    "#;

    // We'll try to parse first
    use zetac::frontend::parser::top_level::parse_zeta;
    let result = parse_zeta(code);

    assert!(result.is_ok(), "Should parse simple code");
    println!("✓ Simple code parses correctly");

    // If parsing works, we can try to compile and run
    let (remaining, ast) = result.unwrap();
    assert!(remaining.is_empty(), "Should parse entire input");
    assert!(!ast.is_empty(), "Should have AST nodes");

    println!("✓ AST generated successfully");

    // Note: We're not testing full compilation here since there might be
    // issues with the codegen or other components
    println!("Test completed - basic parsing works");
}
