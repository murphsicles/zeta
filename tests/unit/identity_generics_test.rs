//! Tests for identity constraints in generics

use zetac::middle::resolver::new_resolver::Resolver;
use zetac::frontend::parser::top_level::parse_zeta;

#[test]
fn test_identity_constraint_parsing() {
    let code = r#"
    fn process_string<T: Identity<Read>>(s: T) -> i64 {
        s.len()
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let ast = result.unwrap();
    println!("AST: {:?}", ast);
}

#[test]
fn test_multiple_capability_constraints() {
    let code = r#"
    fn read_write_processor<T: Identity<Read+Write>>(data: T) -> T {
        data
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_identity_constrained_struct() {
    let code = r#"
    struct SecureContainer<T: Identity<Read>> {
        contents: T,
        access_count: i64,
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_combined_constraints() {
    let code = r#"
    fn process_and_clone<T: Identity<Read> + Clone>(item: T) -> T {
        item.clone()
    }
    "#;
    
    let result = parse_zeta(code);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}