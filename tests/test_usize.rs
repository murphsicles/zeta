//! Test for usize type support in Zeta type system

use zetac::middle::types::*;

#[test]
fn test_usize_type_creation() {
    // Test Type::from_string for usize
    let usize_type = Type::from_string("usize");
    assert_eq!(usize_type, Type::Usize);
    
    // Test display_name
    assert_eq!(usize_type.display_name(), "usize");
    
    // Test mangled_name
    assert_eq!(usize_type.mangled_name(), "usize");
}

#[test]
fn test_usize_unification() {
    let mut subst = Substitution::new();
    
    // Usize should unify with itself
    assert!(subst.unify(&Type::Usize, &Type::Usize).is_ok());
    
    // Usize should unify with i64 (PrimeZeta compatibility hack)
    // Note: This is unsafe but needed for v0.3.26 compatibility
    assert!(subst.unify(&Type::Usize, &Type::I64).is_ok());
    assert!(subst.unify(&Type::I64, &Type::Usize).is_ok());
    
    // Usize should not unify with other types (except i64 and u64 via compatibility hacks)
    assert!(subst.unify(&Type::Usize, &Type::U32).is_err());
    assert!(subst.unify(&Type::Usize, &Type::I32).is_err());
    
    // Usize should unify with u64? Let's check...
    // Actually, the compatibility hack only allows i64 <-> usize and i64 <-> u64
    // usize and u64 don't unify directly
    assert!(subst.unify(&Type::Usize, &Type::U64).is_err());
}

#[test]
fn test_usize_trait_bounds() {
    let subst = Substitution::new();
    
    // Usize should satisfy common trait bounds
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::Copy));
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::Clone));
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::Debug));
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::Default));
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::PartialEq));
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::Eq));
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::PartialOrd));
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::Ord));
    assert!(subst.satisfies_bound(&Type::Usize, &TraitBound::Hash));
}

#[test]
fn test_usize_in_compound_types() {
    // Test usize in array type (Rust syntax: [T; N])
    // Note: Type::from_string doesn't handle PrimeZeta syntax [N]T
    // The parser converts [N]T to [T; N] before calling Type::from_string
    let array_type = Type::from_string("[usize; 10]");
    match array_type {
        Type::Array(inner, size) => {
            assert_eq!(*inner, Type::Usize);
            assert_eq!(size, zetac::middle::types::ArraySize::Literal(10));
        }
        _ => panic!("Expected Array type, got {:?}", array_type),
    }
    
    // Test usize in tuple type
    let tuple_type = Type::from_string("(usize, i64, bool)");
    match tuple_type {
        Type::Tuple(types) => {
            assert_eq!(types.len(), 3);
            assert_eq!(types[0], Type::Usize);
            assert_eq!(types[1], Type::I64);
            assert_eq!(types[2], Type::Bool);
        }
        _ => panic!("Expected Tuple type"),
    }
    
    // Test usize in reference type
    let ref_type = Type::from_string("&usize");
    match ref_type {
        Type::Ref(inner, lifetime, mutability) => {
            assert_eq!(*inner, Type::Usize);
            assert_eq!(lifetime, Lifetime::Static);
            assert_eq!(mutability, Mutability::Immutable);
        }
        _ => panic!("Expected Ref type"),
    }
    
    // Test mutable reference to usize
    let mut_ref_type = Type::from_string("&mut usize");
    match mut_ref_type {
        Type::Ref(inner, lifetime, mutability) => {
            assert_eq!(*inner, Type::Usize);
            assert_eq!(lifetime, Lifetime::Static);
            assert_eq!(mutability, Mutability::Mutable);
        }
        _ => panic!("Expected Ref type"),
    }
}

#[test]
fn test_primezeta_compatibility() {
    // Test the specific PrimeZeta case: const NUM_RESIDUES: usize = 5760
    // This should parse and type-check
    
    // First, test that usize is recognized as a type
    let usize_type = Type::from_string("usize");
    assert_eq!(usize_type, Type::Usize);
    
    // Test that usize can be used in array types
    let array_type = Type::from_string("[usize; 5760]");
    match array_type {
        Type::Array(inner, size) => {
            assert_eq!(*inner, Type::Usize);
            assert_eq!(size, zetac::middle::types::ArraySize::Literal(5760));
        }
        _ => panic!("Expected Array type for [usize; 5760]"),
    }
    
    println!("PrimeZeta compatibility: usize type is recognized and can be used in arrays");
}

#[test]
fn test_all_primitive_types_unify() {
    let mut subst = Substitution::new();
    
    // Test that all primitive types unify with themselves
    let primitive_types = vec![
        Type::I8, Type::I16, Type::I32, Type::I64,
        Type::U8, Type::U16, Type::U32, Type::U64, Type::Usize,
        Type::F32, Type::F64,
        Type::Bool, Type::Char, Type::Str, Type::Range,
    ];
    
    for ty in primitive_types {
        assert!(subst.unify(&ty, &ty).is_ok(), 
                "Type {:?} should unify with itself", ty);
    }
}