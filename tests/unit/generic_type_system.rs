//! Comprehensive test suite for Zeta's generic type system
//!
//! Tests all aspects of generic type handling:
//! 1. Basic generics: Vec<T>, Option<T>, Result<T, E>
//! 2. Generic functions: fn identity<T>(x: T) -> T
//! 3. Trait bounds: T: Clone + Display
//! 4. Lifetimes: struct Ref<'a, T>
//! 5. Where clauses: where T: Eq
//! 6. Reference types: &T, &mut T
//! 7. Complex patterns: Nested generics, recursive types

use zetac::middle::types::{Lifetime, LifetimeVar, Mutability, Substitution, Type, TypeVar};

/// Test basic generic type instantiation
#[test]
fn test_basic_generic_instantiation() {
    // Test Vec<T>
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_type = Type::Named("Vec".to_string(), vec![t_var.clone()]);

    // Instantiate Vec<T> with i32
    let instantiated = vec_type.instantiate_generic(&[Type::I32]);
    assert!(instantiated.is_ok());

    let instantiated_type = instantiated.unwrap();
    match instantiated_type {
        Type::Named(name, args) => {
            assert_eq!(name, "Vec");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], Type::I32);
        }
        _ => panic!("Expected Named type, got {:?}", instantiated_type),
    }

    // Test Option<T>
    let option_type = Type::Named("Option".to_string(), vec![t_var.clone()]);
    let instantiated = option_type.instantiate_generic(&[Type::Bool]);
    assert!(instantiated.is_ok());

    // Test Result<T, E>
    let _t_var2 = Type::Variable(TypeVar::fresh());
    let e_var = Type::Variable(TypeVar::fresh());
    let result_type = Type::Named("Result".to_string(), vec![t_var.clone(), e_var.clone()]);

    let instantiated =
        result_type.instantiate_generic(&[Type::I32, Type::Named("String".to_string(), vec![])]);
    assert!(instantiated.is_ok());

    let instantiated_type = instantiated.unwrap();
    match instantiated_type {
        Type::Named(name, args) => {
            assert_eq!(name, "Result");
            assert_eq!(args.len(), 2);
            assert_eq!(args[0], Type::I32);
            assert_eq!(args[1], Type::Named("String".to_string(), vec![]));
        }
        _ => panic!("Expected Named type, got {:?}", instantiated_type),
    }
}

/// Test generic function type unification
#[test]
fn test_generic_function_unification() {
    let mut subst = Substitution::new();

    // Create a generic function type: fn<T>(T) -> T
    let t_var = Type::Variable(TypeVar::fresh());
    let generic_func = Type::Function(vec![t_var.clone()], Box::new(t_var.clone()));

    // Create a concrete function type: fn(i32) -> i32
    let concrete_func = Type::Function(vec![Type::I32], Box::new(Type::I32));

    // Unify generic function with concrete function
    assert!(subst.unify(&generic_func, &concrete_func).is_ok());

    // Check that T was unified with i32
    assert_eq!(subst.apply(&t_var), Type::I32);

    // Test with different return type (should fail)
    let t_var2 = Type::Variable(TypeVar::fresh());
    let generic_func2 = Type::Function(vec![t_var2.clone()], Box::new(t_var2.clone()));

    let mismatched_func = Type::Function(vec![Type::I32], Box::new(Type::Bool));

    assert!(subst.unify(&generic_func2, &mismatched_func).is_err());
}

/// Test trait bounds in generic parameters
#[test]
fn test_generic_with_trait_bounds() {
    // Note: Trait bounds are represented in the AST but not yet in the type system
    // This test verifies that generic parameters with bounds can be parsed and represented

    // For now, test that we can create and instantiate generic types
    // In the future, trait bounds will be checked during type checking

    let t_var = Type::Variable(TypeVar::fresh());
    let container_type = Type::Named("Container".to_string(), vec![t_var.clone()]);

    // Instantiate Container<T> with Display bound
    let instantiated =
        container_type.instantiate_generic(&[Type::Named("String".to_string(), vec![])]);
    assert!(instantiated.is_ok());

    // String should satisfy Display bound (would be checked in type checker)
    let instantiated_type = instantiated.unwrap();
    assert!(matches!(instantiated_type, Type::Named(_, _)));
}

/// Test lifetime parameters in generic types
#[test]
fn test_lifetime_generics() {
    // Create a type with lifetime: Ref<'a, T>
    let lifetime_var = Lifetime::Variable(LifetimeVar::fresh());
    let t_var = Type::Variable(TypeVar::fresh());

    // Create Ref<'a, T> type
    let ref_type = Type::Ref(
        Box::new(t_var.clone()),
        lifetime_var.clone(),
        Mutability::Immutable,
    );

    // Create a concrete reference type: &'a i32
    let concrete_ref = Type::Ref(
        Box::new(Type::I32),
        lifetime_var.clone(),
        Mutability::Immutable,
    );

    // Unify the generic reference with the concrete reference
    let mut subst = Substitution::new();
    assert!(subst.unify(&ref_type, &concrete_ref).is_ok());

    // Apply substitution to get instantiated type
    let instantiated = subst.apply(&ref_type);

    match instantiated {
        Type::Ref(inner, lifetime, mutability) => {
            assert_eq!(*inner, Type::I32);
            assert_eq!(lifetime, lifetime_var);
            assert_eq!(mutability, Mutability::Immutable);
        }
        _ => panic!("Expected Ref type, got {:?}", instantiated),
    }
}

/// Test where clauses (simplified representation)
#[test]
fn test_where_clauses_generics() {
    // Where clauses are syntactic sugar for trait bounds
    // They're handled during parsing and transformed into trait bounds

    // For the type system, where clauses don't add new semantics
    // This test ensures that types with where clauses can be represented

    let t_var = Type::Variable(TypeVar::fresh());
    let eq_type = Type::Named("EqType".to_string(), vec![t_var.clone()]);

    // EqType<T> where T: Eq
    // In the type system, this is just EqType<T>
    // The Eq bound would be checked separately

    let instantiated = eq_type.instantiate_generic(&[Type::I32]);
    assert!(instantiated.is_ok());

    // i32 should satisfy Eq bound
    let instantiated_type = instantiated.unwrap();
    assert!(matches!(instantiated_type, Type::Named(_, _)));
}

/// Test reference types with generics
#[test]
fn test_reference_types_with_generics() {
    // Test &T where T is generic
    let t_var = Type::Variable(TypeVar::fresh());
    let ref_type = Type::Ref(
        Box::new(t_var.clone()),
        Lifetime::Static,
        Mutability::Immutable,
    );

    // Create a concrete reference: &i32
    let concrete_ref = Type::Ref(Box::new(Type::I32), Lifetime::Static, Mutability::Immutable);

    // Unify to create substitution: T = i32
    let mut subst = Substitution::new();
    assert!(subst.unify(&ref_type, &concrete_ref).is_ok());

    let instantiated = subst.apply(&ref_type);

    match instantiated {
        Type::Ref(inner, lifetime, mutability) => {
            assert_eq!(*inner, Type::I32);
            assert_eq!(lifetime, Lifetime::Static);
            assert_eq!(mutability, Mutability::Immutable);
        }
        _ => panic!("Expected Ref type, got {:?}", instantiated),
    }

    // Test &mut T
    let t_var2 = Type::Variable(TypeVar::fresh());
    let mut_ref_type = Type::Ref(
        Box::new(t_var2.clone()),
        Lifetime::Static,
        Mutability::Mutable,
    );

    // Create a concrete mutable reference: &mut i32
    let concrete_mut_ref = Type::Ref(Box::new(Type::I32), Lifetime::Static, Mutability::Mutable);

    // Unify to create substitution
    let mut subst2 = Substitution::new();
    assert!(subst2.unify(&mut_ref_type, &concrete_mut_ref).is_ok());

    let instantiated = subst2.apply(&mut_ref_type);

    match instantiated {
        Type::Ref(inner, lifetime, mutability) => {
            assert_eq!(*inner, Type::I32);
            assert_eq!(lifetime, Lifetime::Static);
            assert_eq!(mutability, Mutability::Mutable);
        }
        _ => panic!("Expected Ref type, got {:?}", instantiated),
    }
}

/// Test complex nested generic patterns
#[test]
fn test_nested_generics() {
    // Test Vec<Option<T>> - we need to create Vec<T> and then substitute T with Option<U>
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_type = Type::Named("Vec".to_string(), vec![t_var.clone()]);

    // Create Option<i32>
    let option_i32 = Type::Named("Option".to_string(), vec![Type::I32]);

    // Instantiate Vec<T> with T = Option<i32>
    let instantiated = vec_type.instantiate_generic(std::slice::from_ref(&option_i32));
    assert!(instantiated.is_ok());

    let instantiated_type = instantiated.unwrap();
    match instantiated_type {
        Type::Named(name, args) => {
            assert_eq!(name, "Vec");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], option_i32);
        }
        _ => panic!("Expected Named type, got {:?}", instantiated_type),
    }

    // Test HashMap<String, Vec<i32>>
    let k_var = Type::Variable(TypeVar::fresh());
    let v_var = Type::Variable(TypeVar::fresh());
    let hashmap_type = Type::Named("HashMap".to_string(), vec![k_var.clone(), v_var.clone()]);

    let vec_i32 = Type::Named("Vec".to_string(), vec![Type::I32]);
    let instantiated = hashmap_type
        .instantiate_generic(&[Type::Named("String".to_string(), vec![]), vec_i32.clone()]);

    assert!(instantiated.is_ok());
}

/// Test recursive generic types
#[test]
fn test_recursive_generic_types() {
    // Test recursive type like List<T> = Cons(T, Box<List<T>>) | Nil
    // This is simplified - in reality recursive types need special handling

    let t_var = Type::Variable(TypeVar::fresh());
    let list_type = Type::Named("List".to_string(), vec![t_var.clone()]);

    // Box<List<T>> - a common pattern for recursive types
    let _boxed_list = Type::Ptr(Box::new(list_type.clone()), Mutability::Immutable);

    // Cons(T, Box<List<T>>) would be a struct/tuple containing T and Box<List<T>>
    // For now, just test that we can create the type

    // Instantiate List<T> with i32
    let instantiated = list_type.instantiate_generic(&[Type::I32]);
    assert!(instantiated.is_ok());

    // The instantiated type should be List<i32>
    let instantiated_type = instantiated.unwrap();
    assert!(matches!(instantiated_type, Type::Named(_, _)));
}

/// Test generic type unification with variance
#[test]
fn test_generic_unification_variance() {
    let mut subst = Substitution::new();

    // Test that Option<T> can unify with Some<T>
    let t_var = Type::Variable(TypeVar::fresh());
    let option_type = Type::Named("Option".to_string(), vec![t_var.clone()]);
    let some_type = Type::Named("Some".to_string(), vec![Type::I32]);

    // Option<T> should unify with Some<i32> (T = i32)
    assert!(subst.unify(&option_type, &some_type).is_ok());
    assert_eq!(subst.apply(&t_var), Type::I32);

    // Test that Result<T, E> can unify with Ok<T, E>
    let t_var2 = Type::Variable(TypeVar::fresh());
    let e_var = Type::Variable(TypeVar::fresh());
    let result_type = Type::Named("Result".to_string(), vec![t_var2.clone(), e_var.clone()]);
    let ok_type = Type::Named(
        "Ok".to_string(),
        vec![Type::I32, Type::Named("String".to_string(), vec![])],
    );

    assert!(subst.unify(&result_type, &ok_type).is_ok());
    assert_eq!(subst.apply(&t_var2), Type::I32);
    assert_eq!(
        subst.apply(&e_var),
        Type::Named("String".to_string(), vec![])
    );
}

/// Test error cases in generic instantiation
#[test]
fn test_generic_instantiation_errors() {
    // Test wrong number of type arguments
    let t_var = Type::Variable(TypeVar::fresh());
    let e_var = Type::Variable(TypeVar::fresh());
    let result_type = Type::Named("Result".to_string(), vec![t_var, e_var]);

    // Try to instantiate with only one argument (should fail)
    let instantiated = result_type.instantiate_generic(&[Type::I32]);
    assert!(instantiated.is_err());

    let error = instantiated.unwrap_err();
    assert!(error.contains("Wrong number of type arguments"));

    // Test with too many arguments
    let instantiated = result_type.instantiate_generic(&[Type::I32, Type::Bool, Type::I64]);
    assert!(instantiated.is_err());

    let error = instantiated.unwrap_err();
    assert!(error.contains("Wrong number of type arguments"));
}

/// Test generic type display
#[test]
fn test_generic_type_display() {
    // Test Vec<T> - don't check exact counter value since it's non-deterministic
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_type = Type::Named("Vec".to_string(), vec![t_var]);
    let display = vec_type.display_name();
    assert!(display.starts_with("Vec<T") && display.ends_with(">"));

    // Test Result<T, E>
    let t_var = Type::Variable(TypeVar::fresh());
    let e_var = Type::Variable(TypeVar::fresh());
    let result_type = Type::Named("Result".to_string(), vec![t_var, e_var]);
    let display = result_type.display_name();
    assert!(display.starts_with("Result<T") && display.contains(", T") && display.ends_with(">"));

    // Test instantiated type display
    let instantiated = Type::Named("Vec".to_string(), vec![Type::I32]);
    assert_eq!(instantiated.display_name(), "Vec<i32>");

    // Test nested generic display
    let nested = Type::Named(
        "Vec".to_string(),
        vec![Type::Named("Option".to_string(), vec![Type::I32])],
    );
    assert_eq!(nested.display_name(), "Vec<Option<i32>>");
}

/// Test that type variables are properly tracked in generic types
#[test]
fn test_type_variable_tracking() {
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_type = Type::Named("Vec".to_string(), vec![t_var.clone()]);

    // Vec<T> should contain type variables
    assert!(vec_type.contains_vars());

    // Vec<i32> should not contain type variables
    let concrete_vec = Type::Named("Vec".to_string(), vec![Type::I32]);
    assert!(!concrete_vec.contains_vars());

    // Nested generic with variables
    let nested = Type::Named(
        "HashMap".to_string(),
        vec![
            Type::Named("String".to_string(), vec![]),
            Type::Named("Vec".to_string(), vec![t_var.clone()]),
        ],
    );
    assert!(nested.contains_vars());

    // Fully concrete nested type
    let concrete_nested = Type::Named(
        "HashMap".to_string(),
        vec![
            Type::Named("String".to_string(), vec![]),
            Type::Named("Vec".to_string(), vec![Type::I32]),
        ],
    );
    assert!(!concrete_nested.contains_vars());
}
