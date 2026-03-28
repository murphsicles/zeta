//! Property-based tests for Zeta type system
//! 
//! These tests verify invariants and properties of the type system
//! using property-based testing with proptest.

use proptest::prelude::*;
use zetac::middle::types::{Type, TypeVar, Substitution, Mutability};

prop_compose! {
    /// Generate a primitive type
    fn primitive_type()(ty in prop_oneof![
        Just(Type::I8),
        Just(Type::I16),
        Just(Type::I32),
        Just(Type::I64),
        Just(Type::U8),
        Just(Type::U16),
        Just(Type::U32),
        Just(Type::U64),
        Just(Type::F32),
        Just(Type::F64),
        Just(Type::Bool),
        Just(Type::Char),
        Just(Type::Str),
    ]) -> Type {
        ty
    }
}

prop_compose! {
    /// Generate a type variable
    fn type_variable()(id in 0u32..1000) -> TypeVar {
        TypeVar(id)
    }
}

prop_compose! {
    /// Generate a random type (limited depth to avoid stack overflow)
    fn arbitrary_type(max_depth: u32)(depth in 0..max_depth) -> impl Strategy<Value = Type> {
        let leaf = prop_oneof![
            primitive_type(),
            type_variable().prop_map(Type::Variable),
        ];
        
        if depth == 0 {
            leaf.boxed()
        } else {
            prop_oneof![
                // Leaf types
                leaf.clone(),
                // Array types
                (leaf.clone(), 1usize..10).prop_map(|(inner, size)| Type::Array(Box::new(inner), size)),
                // Slice types
                leaf.clone().prop_map(|inner| Type::Slice(Box::new(inner))),
                // Tuple types (1-3 elements)
                prop::collection::vec(leaf.clone(), 1..4).prop_map(Type::Tuple),
                // Pointer types
                leaf.clone().prop_map(|inner| Type::Ptr(Box::new(inner))),
                // Reference types
                (leaf.clone(), prop_oneof![Just(Mutability::Immutable), Just(Mutability::Mutable)])
                    .prop_map(|(inner, mutability)| Type::Ref(Box::new(inner), mutability)),
            ]).boxed()
        }
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(1000))]
    
    #[test]
    fn test_type_display_roundtrip(_type in arbitrary_type(3)) {
        // Property: Display representation should be parseable (in future)
        // For now, just verify display doesn't panic
        let display = _type.display_name();
        prop_assert!(!display.is_empty());
        
        // Basic sanity checks on display output
        prop_assert!(!display.contains("<?>"), "Error type should not appear in display");
    }
    
    #[test]
    fn test_substitution_idempotent(
        substitution in prop::collection::hash_map(
            type_variable(),
            arbitrary_type(2),
            0..5
        ),
        input_type in arbitrary_type(3)
    ) {
        // Property: Applying substitution twice should be same as once
        let mut subst = Substitution::new();
        for (var, ty) in substitution {
            // Note: Real substitution would use unify, but for property test
            // we'll manually insert (simplified for test)
            // In real code, we'd need to handle occurs check
            subst.mapping.insert(var, ty);
        }
        
        let once = subst.apply(&input_type);
        let twice = subst.apply(&once);
        
        // For types without variables, application should be idempotent
        if !input_type.contains_vars() {
            prop_assert_eq!(once, twice, "Substitution should be idempotent for ground types");
        }
    }
    
    #[test]
    fn test_type_equality_reflexive(_type in arbitrary_type(3)) {
        // Property: Type equality is reflexive
        prop_assert_eq!(_type, _type.clone());
    }
    
    #[test]
    fn test_type_equality_symmetric(
        type1 in arbitrary_type(3),
        type2 in arbitrary_type(3)
    ) {
        // Property: If type1 == type2, then type2 == type1
        if type1 == type2 {
            prop_assert_eq!(type2, type1);
        }
    }
    
    #[test]
    fn test_contains_vars_consistency(
        _type in arbitrary_type(3)
    ) {
        // Property: contains_vars should be consistent with structure
        let has_vars = _type.contains_vars();
        
        match &_type {
            Type::Variable(_) => prop_assert!(has_vars, "Type variable should contain vars"),
            Type::Array(inner, _) => {
                prop_assert_eq!(has_vars, inner.contains_vars(), 
                    "Array contains_vars should match inner type");
            }
            Type::Slice(inner) => {
                prop_assert_eq!(has_vars, inner.contains_vars(),
                    "Slice contains_vars should match inner type");
            }
            Type::Tuple(types) => {
                let expected = types.iter().any(|t| t.contains_vars());
                prop_assert_eq!(has_vars, expected,
                    "Tuple contains_vars should be OR of element types");
            }
            Type::Ptr(inner) => {
                prop_assert_eq!(has_vars, inner.contains_vars(),
                    "Pointer contains_vars should match inner type");
            }
            Type::Ref(inner, _) => {
                prop_assert_eq!(has_vars, inner.contains_vars(),
                    "Reference contains_vars should match inner type");
            }
            Type::Named(_, args) => {
                let expected = args.iter().any(|t| t.contains_vars());
                prop_assert_eq!(has_vars, expected,
                    "Named type contains_vars should be OR of type arguments");
            }
            Type::Function(params, ret) => {
                let expected = params.iter().any(|p| p.contains_vars()) || ret.contains_vars();
                prop_assert_eq!(has_vars, expected,
                    "Function contains_vars should be OR of params and return");
            }
            _ => {
                // Primitive types should not contain vars
                prop_assert!(!has_vars, "Primitive type should not contain vars: {:?}", _type);
            }
        }
    }
    
    #[test]
    fn test_unify_same_type(
        _type in arbitrary_type(2)
    ) {
        // Property: A type should unify with itself
        let mut subst = Substitution::new();
        let result = subst.unify(&_type, &_type);
        
        prop_assert!(result.is_ok(), "Type should unify with itself: {:?}", _type);
    }
    
    #[test]
    fn test_unify_commutative(
        type1 in arbitrary_type(2),
        type2 in arbitrary_type(2)
    ) {
        // Property: Unification should be commutative when it succeeds
        let mut subst1 = Substitution::new();
        let mut subst2 = Substitution::new();
        
        let result1 = subst1.unify(&type1, &type2);
        let result2 = subst2.unify(&type2, &type1);
        
        // If one succeeds, the other should also succeed
        // and produce equivalent substitutions
        match (result1, result2) {
            (Ok(()), Ok(())) => {
                // For ground types, the substitutions should be equivalent
                if !type1.contains_vars() && !type2.contains_vars() {
                    let applied1 = subst1.apply(&type1);
                    let applied2 = subst2.apply(&type2);
                    prop_assert_eq!(applied1, applied2, 
                        "Unification should be commutative for ground types");
                }
            }
            (Err(_), Err(_)) => {
                // Both failed - that's fine
            }
            (Ok(()), Err(_)) | (Err(_), Ok(())) => {
                prop_assert!(false, "Unification should be commutative: one succeeded, other failed");
            }
        }
    }
}

// Test specific unification properties
proptest! {
    #![proptest_config(ProptestConfig::with_cases(500))]
    
    #[test]
    fn test_variable_unification(
        var in type_variable(),
        ty in arbitrary_type(2)
    ) {
        // Property: Variable should unify with any type (except occurs check)
        let var_type = Type::Variable(var.clone());
        let mut subst = Substitution::new();
        
        let result = subst.unify(&var_type, &ty);
        
        // Check if occurs check would fail
        let mut check_subst = Substitution::new();
        let occurs = check_subst.occurs_check(&var, &ty);
        
        if occurs {
            prop_assert!(result.is_err(), 
                "Should fail occurs check when variable occurs in type");
        } else {
            prop_assert!(result.is_ok(), 
                "Should succeed when no occurs check violation");
            
            // After unification, variable should be substituted
            let applied = subst.apply(&var_type);
            prop_assert_ne!(applied, var_type, 
                "Variable should be substituted after unification");
        }
    }
    
    #[test]
    fn test_array_unification(
        inner1 in arbitrary_type(2),
        inner2 in arbitrary_type(2),
        size in 1usize..10
    ) {
        // Property: Arrays unify only if same size and inner types unify
        let array1 = Type::Array(Box::new(inner1.clone()), size);
        let array2 = Type::Array(Box::new(inner2.clone()), size);
        
        let mut subst = Substitution::new();
        let result = subst.unify(&array1, &array2);
        
        // Arrays should unify if inner types unify
        let mut inner_subst = Substitution::new();
        let inner_result = inner_subst.unify(&inner1, &inner2);
        
        match inner_result {
            Ok(()) => {
                prop_assert!(result.is_ok(), 
                    "Arrays should unify if inner types unify");
            }
            Err(_) => {
                prop_assert!(result.is_err(), 
                    "Arrays should not unify if inner types don't unify");
            }
        }
    }
    
    #[test]
    fn test_tuple_unification(
        types1 in prop::collection::vec(arbitrary_type(2), 1..4),
        types2 in prop::collection::vec(arbitrary_type(2), 1..4)
    ) {
        // Property: Tuples unify only if same length and elements unify pairwise
        let tuple1 = Type::Tuple(types1.clone());
        let tuple2 = Type::Tuple(types2.clone());
        
        let mut subst = Substitution::new();
        let result = subst.unify(&tuple1, &tuple2);
        
        if types1.len() != types2.len() {
            prop_assert!(result.is_err(), 
                "Tuples with different lengths should not unify");
        } else {
            // Check if all element pairs unify
            let mut all_unify = true;
            let mut elem_subst = Substitution::new();
            
            for (t1, t2) in types1.iter().zip(types2.iter()) {
                if elem_subst.unify(t1, t2).is_err() {
                    all_unify = false;
                    break;
                }
            }
            
            if all_unify {
                prop_assert!(result.is_ok(), 
                    "Tuples should unify if all elements unify");
            } else {
                prop_assert!(result.is_err(), 
                    "Tuples should not unify if any element doesn't unify");
            }
        }
    }
}

// Test for specific edge cases in the type system
#[test]
fn test_specific_edge_cases() {
    // Test 1: Self-referential type (should fail occurs check)
    let mut subst = Substitution::new();
    let a = Type::Variable(TypeVar(0));
    let list = Type::Named("List".to_string(), vec![a.clone()]);
    
    // a = List<a> should fail
    assert!(subst.unify(&a, &list).is_err());
    
    // Test 2: Nested arrays
    let mut subst = Substitution::new();
    let inner = Type::Variable(TypeVar(1));
    let array1 = Type::Array(Box::new(Type::Array(Box::new(inner.clone()), 5)), 10);
    let array2 = Type::Array(Box::new(Type::Array(Box::new(Type::I32), 5)), 10);
    
    assert!(subst.unify(&array1, &array2).is_ok());
    assert_eq!(subst.apply(&inner), Type::I32);
    
    // Test 3: Function type with variables
    let mut subst = Substitution::new();
    let a = Type::Variable(TypeVar(2));
    let b = Type::Variable(TypeVar(3));
    
    let func1 = Type::Function(vec![a.clone()], Box::new(b.clone()));
    let func2 = Type::Function(vec![Type::I32], Box::new(Type::I64));
    
    assert!(subst.unify(&func1, &func2).is_ok());
    assert_eq!(subst.apply(&a), Type::I32);
    assert_eq!(subst.apply(&b), Type::I64);
}

// Test for display consistency
#[test]
fn test_display_consistency() {
    let test_cases = vec![
        (Type::I64, "i64"),
        (Type::Bool, "bool"),
        (Type::Str, "str"),
        (Type::Array(Box::new(Type::I32), 10), "[i32; 10]"),
        (Type::Tuple(vec![Type::I32, Type::Bool]), "(i32, bool)"),
        (Type::Ptr(Box::new(Type::I32)), "*i32"),
        (Type::Ref(Box::new(Type::I32), Mutability::Immutable), "&i32"),
        (Type::Ref(Box::new(Type::I32), Mutability::Mutable), "&mut i32"),
        (Type::Function(vec![Type::I32, Type::I32], Box::new(Type::I32)), "(i32, i32) -> i32"),
    ];
    
    for (ty, expected) in test_cases {
        assert_eq!(ty.display_name(), expected, 
            "Display mismatch for {:?}", ty);
    }
}