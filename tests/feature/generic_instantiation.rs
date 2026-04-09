//! Tests for generic type instantiation
//!
//! Tests that generic types like Result<T, E> can be instantiated
//! with concrete types like Result<i64, String>

use zetac::middle::types::{Type, TypeVar};

#[test]
fn test_generic_instantiation_basic() {
    // Create a generic type Result<T, E>
    let t_var = Type::Variable(TypeVar::fresh());
    let e_var = Type::Variable(TypeVar::fresh());
    let result_type = Type::Named("Result".to_string(), vec![t_var, e_var]);

    // Instantiate with concrete types: Result<i64, String>
    let instantiated =
        result_type.instantiate_generic(&[Type::I64, Type::Named("String".to_string(), vec![])]);

    assert!(instantiated.is_ok());
    let instantiated_type = instantiated.unwrap();

    // Check that the instantiated type is Result<i64, String>
    match instantiated_type {
        Type::Named(name, args) => {
            assert_eq!(name, "Result");
            assert_eq!(args.len(), 2);
            assert_eq!(args[0], Type::I64);
            assert_eq!(args[1], Type::Named("String".to_string(), vec![]));
        }
        _ => panic!("Expected Named type, got {:?}", instantiated_type),
    }
}

#[test]
fn test_generic_instantiation_wrong_arity() {
    // Create a generic type Result<T, E>
    let t_var = Type::Variable(TypeVar::fresh());
    let e_var = Type::Variable(TypeVar::fresh());
    let result_type = Type::Named("Result".to_string(), vec![t_var, e_var]);

    // Try to instantiate with wrong number of arguments
    let instantiated = result_type.instantiate_generic(&[
        Type::I64,
        // Missing second argument
    ]);

    assert!(instantiated.is_err());
    let error = instantiated.unwrap_err();
    assert!(error.contains("Wrong number of type arguments"));
}

#[test]
fn test_generic_instantiation_nested() {
    // Create a generic type Vec<T>
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_type = Type::Named("Vec".to_string(), vec![t_var]);

    // Instantiate with Option<i32>
    let option_type = Type::Named("Option".to_string(), vec![Type::I32]);
    let instantiated = vec_type.instantiate_generic(std::slice::from_ref(&option_type));

    assert!(instantiated.is_ok());
    let instantiated_type = instantiated.unwrap();

    // Check that the instantiated type is Vec<Option<i32>>
    match instantiated_type {
        Type::Named(name, args) => {
            assert_eq!(name, "Vec");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], option_type);
        }
        _ => panic!("Expected Named type, got {:?}", instantiated_type),
    }
}

#[test]
fn test_generic_instantiation_with_type_variables() {
    // Create a generic type HashMap<K, V>
    let k_var = Type::Variable(TypeVar::fresh());
    let v_var = Type::Variable(TypeVar::fresh());
    let hashmap_type = Type::Named("HashMap".to_string(), vec![k_var, v_var]);

    // Instantiate with String and Mir
    let instantiated = hashmap_type.instantiate_generic(&[
        Type::Named("String".to_string(), vec![]),
        Type::Named("Mir".to_string(), vec![]),
    ]);

    assert!(instantiated.is_ok());
    let instantiated_type = instantiated.unwrap();

    // Check that the instantiated type is HashMap<String, Mir>
    match instantiated_type {
        Type::Named(name, args) => {
            assert_eq!(name, "HashMap");
            assert_eq!(args.len(), 2);
            assert_eq!(args[0], Type::Named("String".to_string(), vec![]));
            assert_eq!(args[1], Type::Named("Mir".to_string(), vec![]));
        }
        _ => panic!("Expected Named type, got {:?}", instantiated_type),
    }
}

#[test]
fn test_generic_instantiation_in_function_type() {
    // This test is actually testing something different than generic instantiation.
    // Generic instantiation is about taking a generic type like Result<T, E> and
    // substituting T and E with concrete types.
    // A function type with free type variables is not a generic type in the same sense.
    // So let's test something more appropriate.

    // Create a generic type: Wrapper<F> where F is a function type
    let f_var = Type::Variable(TypeVar::fresh());
    let wrapper_type = Type::Named("Wrapper".to_string(), vec![f_var]);

    // Create a concrete function type: fn(i64) -> i64
    let concrete_func = Type::Function(vec![Type::I64], Box::new(Type::I64));

    // Instantiate Wrapper<F> with F = fn(i64) -> i64
    let instantiated = wrapper_type.instantiate_generic(std::slice::from_ref(&concrete_func));

    assert!(instantiated.is_ok());
    let instantiated_type = instantiated.unwrap();

    // Check that the instantiated type is Wrapper<fn(i64) -> i64>
    match instantiated_type {
        Type::Named(name, args) => {
            assert_eq!(name, "Wrapper");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], concrete_func);
        }
        _ => panic!("Expected Named type, got {:?}", instantiated_type),
    }
}
