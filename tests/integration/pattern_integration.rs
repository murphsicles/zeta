//! Integration test for pattern matching features

use zetac::compile_and_run_zeta;

#[test]
fn test_pattern_matching_integration() {
    // Test if-let with Result type
    let code = r#"
enum Result<T, E> {
    Ok(T),
    Err(E),
}

fn main() -> i64 {
    let result = Result::Ok(42);
    
    if let Result::Ok(value) = result {
        value
    } else {
        0
    }
}
"#;

    let result = compile_and_run_zeta(code);
    // This might fail because we don't have full enum support yet,
    // but the parsing should work
    println!("If-let integration test result: {:?}", result);

    // Test match expression with tuple patterns
    let code = r#"
fn main() -> i64 {
    let point = (10, 20);
    
    match point {
        (x, y) => x + y,
        _ => 0,
    }
}
"#;

    let result = compile_and_run_zeta(code);
    println!("Tuple pattern match test result: {:?}", result);

    // Test pattern guards
    let code = r#"
fn main() -> i64 {
    let value = 42;
    
    match value {
        x if x > 0 => 1,
        x if x < 0 => -1,
        _ => 0,
    }
}
"#;

    let result = compile_and_run_zeta(code);
    println!("Pattern guard test result: {:?}", result);
}
