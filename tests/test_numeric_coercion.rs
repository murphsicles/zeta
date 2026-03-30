//! Tests for numeric type coercions

use zetac::middle::types::{Substitution, Type};

#[test]
fn test_i32_to_i64_coercion() {
    let mut subst = Substitution::new();

    // i32 should NOT unify with i64 (no implicit coercion in unification)
    let i32_type = Type::I32;
    let i64_type = Type::I64;

    assert!(subst.unify(&i32_type, &i64_type).is_err());
}

#[test]
fn test_i64_to_i32_fails() {
    let mut subst = Substitution::new();

    // i64 should NOT unify with i32 (unsafe, can overflow)
    let i64_type = Type::I64;
    let i32_type = Type::I32;

    // This should fail because we only allow i32 -> i64, not i64 -> i32
    assert!(subst.unify(&i64_type, &i32_type).is_err());
}

#[test]
fn test_same_type_unification() {
    let mut subst = Substitution::new();

    // Same types should always unify
    assert!(subst.unify(&Type::I32, &Type::I32).is_ok());
    assert!(subst.unify(&Type::I64, &Type::I64).is_ok());
    assert!(subst.unify(&Type::F32, &Type::F32).is_ok());
    assert!(subst.unify(&Type::F64, &Type::F64).is_ok());
}

#[test]
fn test_no_coercion_between_incompatible_types() {
    let mut subst = Substitution::new();

    // No coercion between incompatible types
    assert!(subst.unify(&Type::I32, &Type::Bool).is_err());
    assert!(subst.unify(&Type::I64, &Type::Str).is_err());
    assert!(subst.unify(&Type::F32, &Type::I32).is_err());
}
