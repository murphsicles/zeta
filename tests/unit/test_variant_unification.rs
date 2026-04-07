//! Tests for variant type unification (Option/Some, Result/Ok/Err)

use zetac::middle::types::{Substitution, Type};

#[test]
fn test_option_some_unification() {
    let mut subst = Substitution::new();

    // Some<i32> should unify with Option<i32>
    let some_i32 = Type::Named("Some".to_string(), vec![Type::I32]);
    let option_i32 = Type::Named("Option".to_string(), vec![Type::I32]);

    assert!(subst.unify(&some_i32, &option_i32).is_ok());
    assert!(subst.unify(&option_i32, &some_i32).is_ok());
}

#[test]
fn test_option_none_unification() {
    let mut subst = Substitution::new();

    // None should unify with Option<T> for any T
    let none = Type::Named("None".to_string(), vec![]);
    let option_i32 = Type::Named("Option".to_string(), vec![Type::I32]);

    assert!(subst.unify(&none, &option_i32).is_ok());
    assert!(subst.unify(&option_i32, &none).is_ok());
}

#[test]
fn test_result_ok_unification() {
    let mut subst = Substitution::new();

    // Ok<i32, String> should unify with Result<i32, String>
    let ok = Type::Named("Ok".to_string(), vec![Type::I32, Type::Str]);
    let result = Type::Named("Result".to_string(), vec![Type::I32, Type::Str]);

    assert!(subst.unify(&ok, &result).is_ok());
    assert!(subst.unify(&result, &ok).is_ok());
}

#[test]
fn test_result_err_unification() {
    let mut subst = Substitution::new();

    // Err<i32, String> should unify with Result<i32, String>
    let err = Type::Named("Err".to_string(), vec![Type::I32, Type::Str]);
    let result = Type::Named("Result".to_string(), vec![Type::I32, Type::Str]);

    assert!(subst.unify(&err, &result).is_ok());
    assert!(subst.unify(&result, &err).is_ok());
}

#[test]
fn test_non_variant_unification_fails() {
    let mut subst = Substitution::new();

    // Point should NOT unify with Option (no variant relationship)
    let point = Type::Named("Point".to_string(), vec![Type::I32, Type::I32]);
    let option = Type::Named("Option".to_string(), vec![Type::I32]);

    assert!(subst.unify(&point, &option).is_err());
    assert!(subst.unify(&option, &point).is_err());
}
