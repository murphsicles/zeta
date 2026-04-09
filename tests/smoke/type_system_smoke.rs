// Type System (SEM) Smoke Tests
// Basic smoke tests for the type system

use zetac::middle::types::{Substitution, Type, TypeVar};

#[test]
fn test_type_system_smoke_basic_types() {
    // Test basic type creation and display
    let types = vec![
        (Type::I32, "i32"),
        (Type::F64, "f64"),
        (Type::Bool, "bool"),
        (Type::Str, "str"),
        // Note: Unit type might be Type::Tuple(vec![]) or similar
    ];

    for (ty, expected_display) in types {
        let display = ty.display_name();
        assert_eq!(
            display, expected_display,
            "Type display mismatch for {:?}",
            ty
        );
    }
}

#[test]
fn test_type_system_smoke_type_variables() {
    // Test type variable creation
    let var1 = TypeVar::fresh();
    let var2 = TypeVar::fresh();

    // Type variables should have different IDs
    assert_ne!(
        format!("{:?}", var1),
        format!("{:?}", var2),
        "Type variables should be distinct"
    );

    // Test creating variable type
    let ty_var = Type::Variable(var1);
    assert!(
        matches!(ty_var, Type::Variable(_)),
        "Should create variable type"
    );
}

#[test]
fn test_type_system_smoke_function_types() {
    // Test function type creation
    let param_types = vec![Type::I32, Type::F64];
    let ret_type = Type::Bool;

    let fn_type = Type::Function(param_types.clone(), Box::new(ret_type.clone()));

    // Function type should display correctly
    let display = fn_type.display_name();
    assert!(
        display.contains("i32"),
        "Function type should include parameter types"
    );
    assert!(
        display.contains("f64"),
        "Function type should include parameter types"
    );
    assert!(
        display.contains("bool"),
        "Function type should include return type"
    );
}

#[test]
fn test_type_system_smoke_substitution_creation() {
    // Test that substitution can be created
    let subst = Substitution::new();

    // Substitution should be created without panic
    println!("Substitution created: {:?}", subst);
}

#[test]
fn test_type_system_smoke_unification_basic() {
    // Test basic type unification
    let mut subst = Substitution::new();

    // Unify int with int should succeed
    let result = subst.unify(&Type::I32, &Type::I32);
    assert!(result.is_ok(), "Should unify i32 with i32");

    // Unify int with float should fail
    let result = subst.unify(&Type::I32, &Type::F64);
    assert!(result.is_err(), "Should not unify i32 with f64");
}

#[test]
fn test_type_system_smoke_type_application() {
    // Test type substitution application
    let mut subst = Substitution::new();
    let var = TypeVar::fresh();
    let var_type = Type::Variable(var.clone());

    // Bind variable to i32
    subst.unify(&var_type, &Type::I32).unwrap();

    // Apply substitution
    let applied = subst.apply(&var_type);
    assert_eq!(
        applied,
        Type::I32,
        "Variable should be substituted with i32"
    );
}
