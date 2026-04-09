// Test for generic function arity fix
use zetac::middle::types::{GenericContext, Substitution, Type, TypeVar};

#[test]
fn test_generic_function_arity() {
    let substitution = Substitution::new();
    let context = GenericContext::new();

    // Test 1: Function with one type variable
    let t_var = Type::Variable(TypeVar(0));
    let func_t = Type::Function(vec![t_var.clone()], Box::new(t_var.clone()));

    // Wrong number of type arguments (2 instead of 1)
    let type_args_wrong = vec![Type::I32, Type::I64];
    let result = substitution.instantiate_generic_with_bounds(&func_t, &type_args_wrong, &context);
    assert!(
        result.is_err(),
        "Should fail with wrong number of type arguments"
    );

    // Correct number of type arguments
    let type_args_correct = vec![Type::I32];
    let result =
        substitution.instantiate_generic_with_bounds(&func_t, &type_args_correct, &context);
    assert!(
        result.is_ok(),
        "Should succeed with correct number of type arguments"
    );

    // Test 2: Function with two type variables
    let t_var1 = Type::Variable(TypeVar(0));
    let t_var2 = Type::Variable(TypeVar(1));
    let func_t_u = Type::Function(
        vec![t_var1.clone(), t_var2.clone()],
        Box::new(t_var1.clone()),
    );

    // Wrong number of type arguments (1 instead of 2)
    let result = substitution.instantiate_generic_with_bounds(&func_t_u, &[Type::I32], &context);
    assert!(
        result.is_err(),
        "Should fail with wrong number of type arguments for 2-param function"
    );

    // Correct number of type arguments
    let result =
        substitution.instantiate_generic_with_bounds(&func_t_u, &[Type::I32, Type::Bool], &context);
    assert!(
        result.is_ok(),
        "Should succeed with correct number of type arguments for 2-param function"
    );

    // Test 3: Named type with type arguments
    let option_t = Type::Named("Option".to_string(), vec![t_var1.clone()]);

    // Wrong arity for Option<T>
    let result =
        substitution.instantiate_generic_with_bounds(&option_t, &[Type::I32, Type::I64], &context);
    assert!(result.is_err(), "Option should fail with wrong arity");

    // Correct arity for Option<T>
    let result = substitution.instantiate_generic_with_bounds(&option_t, &[Type::I32], &context);
    assert!(result.is_ok(), "Option should succeed with correct arity");
}

#[test]
fn test_type_mismatch_errors() {
    let mut substitution = Substitution::new();

    // Test string vs i64 error
    let result = substitution.unify(&Type::Str, &Type::I64);
    assert!(result.is_err(), "String vs i64 should fail");
    let error = result.unwrap_err();
    let error_msg = error.to_string();
    assert!(
        error_msg.contains("string as integer"),
        "Error message should mention string/integer mismatch"
    );

    // Test i64 vs string error
    let result = substitution.unify(&Type::I64, &Type::Str);
    assert!(result.is_err(), "i64 vs string should fail");
    let error = result.unwrap_err();
    let error_msg = error.to_string();
    assert!(
        error_msg.contains("string as integer"),
        "Error message should mention string/integer mismatch"
    );

    // Test bool vs i64 error
    let result = substitution.unify(&Type::Bool, &Type::I64);
    assert!(result.is_err(), "Bool vs i64 should fail");
    let error = result.unwrap_err();
    let error_msg = error.to_string();
    assert!(
        error_msg.contains("boolean as integer"),
        "Error message should mention boolean/integer mismatch"
    );

    // Test i64 vs f64 error
    let result = substitution.unify(&Type::I64, &Type::F64);
    assert!(result.is_err(), "i64 vs f64 should fail");
    let error = result.unwrap_err();
    let error_msg = error.to_string();
    assert!(
        error_msg.contains("integer and floating-point"),
        "Error message should mention integer/float mismatch"
    );
}
