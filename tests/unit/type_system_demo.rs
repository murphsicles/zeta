//! Demonstration of new type system capabilities

use zetac::middle::types::{Substitution, Type, TypeVar};

#[test]
fn demonstrate_new_type_system() {
    println!("=== Demonstrating New Type System Foundation ===");

    // Test 1: Type representation
    println!("\n1. Algebraic Type Representation (not strings):");
    let i64_type = Type::I64;
    let bool_type = Type::Bool;
    let array_type = Type::Array(Box::new(Type::I32), 10);
    let tuple_type = Type::Tuple(vec![Type::I32, Type::Bool]);
    let func_type = Type::Function(vec![Type::I32, Type::I32], Box::new(Type::I32));

    println!("  i64: {}", i64_type.display_name());
    println!("  bool: {}", bool_type.display_name());
    println!("  f32: {}", Type::F32.display_name());
    println!("  f64: {}", Type::F64.display_name());
    println!("  [i32; 10]: {}", array_type.display_name());
    println!("  (i32, bool): {}", tuple_type.display_name());
    println!("  (i32, i32) -> i32: {}", func_type.display_name());

    // Test 2: Type variable unification
    println!("\n2. Type Variable Unification:");
    let mut subst = Substitution::new();
    let a = Type::Variable(TypeVar::fresh());
    let b = Type::Variable(TypeVar::fresh());

    println!("  Variables: {} and {}", a.display_name(), b.display_name());

    // Unify a = i64
    assert!(subst.unify(&a, &Type::I64).is_ok());
    println!("  ✓ {} = i64", a.display_name());

    // Unify b = a (which is now i64)
    assert!(subst.unify(&b, &a).is_ok());
    println!("  ✓ {} = {}", b.display_name(), a.display_name());

    // Verify substitution
    assert_eq!(subst.apply(&a), Type::I64);
    assert_eq!(subst.apply(&b), Type::I64);
    println!("  After substitution: both are i64");

    // Test 3: Function type inference
    println!("\n3. Function Type Inference:");
    let mut subst2 = Substitution::new();
    let x = Type::Variable(TypeVar::fresh());

    // (i32) -> X  unified with  (i32) -> i64
    let func1 = Type::Function(vec![Type::I32], Box::new(x.clone()));
    let func2 = Type::Function(vec![Type::I32], Box::new(Type::I64));

    assert!(subst2.unify(&func1, &func2).is_ok());
    assert_eq!(subst2.apply(&x), Type::I64);
    println!("  ✓ Inferred X = i64 in function type");

    // Test 4: Occurs check prevents infinite types
    println!("\n4. Occurs Check (Soundness):");
    let mut subst3 = Substitution::new();
    let y = Type::Variable(TypeVar::fresh());
    let list_of_y = Type::Named("List".to_string(), vec![y.clone()]);

    // y = List<y> should fail occurs check
    assert!(subst3.unify(&y, &list_of_y).is_err());
    println!(
        "  ✓ Correctly rejected infinite type: {} = {}",
        y.display_name(),
        list_of_y.display_name()
    );

    // Test 5: Complex unification
    println!("\n5. Complex Unification Example:");
    let mut subst4 = Substitution::new();
    let t1 = Type::Variable(TypeVar::fresh());
    let t2 = Type::Variable(TypeVar::fresh());

    // Constraint: (t1, i32) -> t2  =  (i64, i32) -> bool
    let left = Type::Function(vec![t1.clone(), Type::I32], Box::new(t2.clone()));
    let right = Type::Function(vec![Type::I64, Type::I32], Box::new(Type::Bool));

    assert!(subst4.unify(&left, &right).is_ok());
    assert_eq!(subst4.apply(&t1), Type::I64);
    assert_eq!(subst4.apply(&t2), Type::Bool);
    println!("  ✓ Inferred: t1 = i64, t2 = bool");

    // Test 6: Float type unification
    println!("\n6. Float Type Support:");
    let mut subst5 = Substitution::new();

    // Float literals unify with f64
    assert!(subst5.unify(&Type::F64, &Type::F64).is_ok());
    println!("  ✓ f64 = f64 unification works");

    // f32 unifies with f32
    assert!(subst5.unify(&Type::F32, &Type::F32).is_ok());
    println!("  ✓ f32 = f32 unification works");

    // f32 != f64 (different types)
    assert!(subst5.unify(&Type::F32, &Type::F64).is_err());
    println!("  ✓ f32 != f64 (distinct types)");

    // Variable can unify with float type
    let float_var = Type::Variable(TypeVar::fresh());
    assert!(subst5.unify(&float_var, &Type::F64).is_ok());
    assert_eq!(subst5.apply(&float_var), Type::F64);
    println!("  ✓ Variable can unify with f64");

    println!("\n=== New Type System Features Verified ===");
    println!("All foundational components working correctly!");
}

#[test]
fn test_backward_compatibility_shim() {
    println!("\n=== Testing Backward Compatibility ===");

    // The old system used type Type = String
    // New system provides conversion utilities

    use zetac::middle::resolver::resolver::Resolver;
    use zetac::middle::resolver::typecheck_new::NewTypeCheck;

    let resolver = Resolver::new();

    // Convert old string types to new Type enum
    assert_eq!(resolver.string_to_type("i64"), Type::I64);
    assert_eq!(resolver.string_to_type("bool"), Type::Bool);
    assert_eq!(resolver.string_to_type("str"), Type::Str);
    assert_eq!(resolver.string_to_type("f32"), Type::F32);
    assert_eq!(resolver.string_to_type("f64"), Type::F64);

    // Convert new Type to old string representation
    assert_eq!(resolver.type_to_string(&Type::I64), "i64");
    assert_eq!(resolver.type_to_string(&Type::Bool), "bool");
    assert_eq!(resolver.type_to_string(&Type::Str), "str");
    assert_eq!(resolver.type_to_string(&Type::F32), "f32");
    assert_eq!(resolver.type_to_string(&Type::F64), "f64");

    println!("  ✓ String <-> Type conversions work");
    println!("  ✓ Migration path available");
}
