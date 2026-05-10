//! Tests for derive attribute handling
//!
//! Tests that #[derive(Copy, Clone, Debug)] attributes are processed correctly

use zetac::middle::types::handle_derive_attribute;

#[test]
fn test_derive_copy() {
    let attr = "derive(Copy)";
    let type_name = "Point";

    let result = handle_derive_attribute(attr, type_name);
    assert!(result.is_ok());

    let implementations = result.unwrap();
    assert_eq!(implementations.len(), 1);
    assert!(implementations[0].contains("impl Copy for Point"));
}

#[test]
fn test_derive_clone() {
    let attr = "derive(Clone)";
    let type_name = "Point";

    let result = handle_derive_attribute(attr, type_name);
    assert!(result.is_ok());

    let implementations = result.unwrap();
    assert_eq!(implementations.len(), 1);
    assert!(implementations[0].contains("impl Clone for Point"));
    assert!(implementations[0].contains("fn clone"));
}

#[test]
fn test_derive_multiple_traits() {
    let attr = "derive(Copy, Clone, Debug)";
    let type_name = "Point";

    let result = handle_derive_attribute(attr, type_name);
    assert!(result.is_ok());

    let implementations = result.unwrap();
    assert_eq!(implementations.len(), 3);

    // Check that all three traits are implemented
    let impl_text = implementations.join("\n");
    assert!(impl_text.contains("impl Copy for Point"));
    assert!(impl_text.contains("impl Clone for Point"));
    assert!(impl_text.contains("impl Debug for Point"));
}

#[test]
fn test_derive_eq() {
    let attr = "derive(Eq)";
    let type_name = "Point";

    let result = handle_derive_attribute(attr, type_name);
    assert!(result.is_ok());

    let implementations = result.unwrap();
    assert_eq!(implementations.len(), 1);
    assert!(implementations[0].contains("Eq is a marker trait"));
}

#[test]
fn test_derive_partial_eq() {
    let attr = "derive(PartialEq)";
    let type_name = "Point";

    let result = handle_derive_attribute(attr, type_name);
    assert!(result.is_ok());

    let implementations = result.unwrap();
    assert_eq!(implementations.len(), 1);
    assert!(implementations[0].contains("impl PartialEq for Point"));
    assert!(implementations[0].contains("fn eq"));
}

#[test]
fn test_derive_unsupported_trait() {
    let attr = "derive(Serialize)";
    let type_name = "Point";

    let result = handle_derive_attribute(attr, type_name);
    assert!(result.is_err());

    let error = result.unwrap_err();
    assert!(error.contains("Unsupported derive trait"));
}

#[test]
fn test_not_a_derive_attribute() {
    let attr = "inline";
    let type_name = "Point";

    let result = handle_derive_attribute(attr, type_name);
    assert!(result.is_err());

    let error = result.unwrap_err();
    assert!(error.contains("Not a derive attribute"));
}

#[test]
fn test_derive_with_spaces() {
    let attr = "derive(Copy, Clone, Debug)";
    let type_name = "MyStruct";

    let result = handle_derive_attribute(attr, type_name);
    assert!(result.is_ok());

    let implementations = result.unwrap();
    assert_eq!(implementations.len(), 3);

    // Check all implementations are generated
    assert!(
        implementations
            .iter()
            .any(|s| s.contains("impl Copy for MyStruct"))
    );
    assert!(
        implementations
            .iter()
            .any(|s| s.contains("impl Clone for MyStruct"))
    );
    assert!(
        implementations
            .iter()
            .any(|s| s.contains("impl Debug for MyStruct"))
    );
}
