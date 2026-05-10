//! Tests for identity-aware pattern matching

extern crate zetac;

use zetac::frontend::parser::top_level::parse_zeta;

#[test]
fn test_type_annotated_pattern_parsing() {
    // Test simple type-annotated pattern
    let code = "fn test(x: i64) -> i64 { match x { y: i64 => y, _ => 0 } }";
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Failed to parse type-annotated pattern: {:?}", result);
    
    // Test identity type annotation
    let code2 = "fn test(s: string) -> i64 { match s { t: string[identity:read] => 1, _ => 0 } }";
    let result2 = parse_zeta(code2);
    assert!(result2.is_ok(), "Failed to parse identity type annotation in pattern: {:?}", result2);
}

#[test]
fn test_identity_pattern_in_let() {
    // Test let binding with identity type
    let code = "fn test() -> i64 { let s: string[identity:read] = \"hello\"; 42 }";
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Failed to parse let binding with identity type: {:?}", result);
}