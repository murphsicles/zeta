use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test 1: Malformed generic (missing closing >)
    let bad_code = r#"
    struct Bad< { // Missing closing >
    "#;

    let result = parse_zeta(bad_code);
    println!("Test 1 - Malformed generic:");
    println!("  Result: {:?}", result);
    println!("  Is error: {}", result.is_err());

    // Test 2: Well-formed generic
    let good_code = r#"
    struct Good<T> { x: T }
    "#;

    let result = parse_zeta(good_code);
    println!("\nTest 2 - Well-formed generic:");
    println!("  Result: {:?}", result);
    println!("  Is ok: {}", result.is_ok());

    // Test 3: Check what the parser actually returns
    if let Ok((remaining, ast)) = parse_zeta(good_code) {
        println!("\nTest 3 - Parsed AST:");
        println!("  Remaining: '{}'", remaining);
        println!("  AST nodes: {}", ast.len());
        for node in ast {
            println!("    {:?}", node);
        }
    }
}
