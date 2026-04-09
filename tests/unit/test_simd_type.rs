// Test SIMD type system
use zetac::middle::types::Type;

#[test]
fn test_vector_type() {
    // Create a vector type
    let element_type = Box::new(Type::I32);
    let vector_type = Type::Vector(element_type, zetac::middle::types::ArraySize::Literal(4));
    
    // Test display name
    assert_eq!(vector_type.display_name(), "Vector<i32, 4>");
    
    // Test is_vector method
    assert!(vector_type.is_vector());
    
    // Test as_vector method
    if let Some((inner, size)) = vector_type.as_vector() {
        assert_eq!(*inner, Type::I32);
        assert_eq!(size, 4);
    } else {
        panic!("as_vector should return Some for vector types");
    }
    
    // Test mangled name
    assert_eq!(vector_type.mangled_name(), "Vector_i32_4");
    
    // Test that non-vector types return false for is_vector
    let int_type = Type::I32;
    assert!(!int_type.is_vector());
    assert!(int_type.as_vector().is_none());
}

#[test]
fn test_vector_unification() {
    use zetac::middle::types::{Substitution, TypeVar};
    
    let mut sub = Substitution::new();
    
    // Two identical vector types should unify
    let vec1 = Type::Vector(Box::new(Type::I32), zetac::middle::types::ArraySize::Literal(4));
    let vec2 = Type::Vector(Box::new(Type::I32), zetac::middle::types::ArraySize::Literal(4));
    
    assert!(sub.unify(&vec1, &vec2).is_ok());
    
    // Different sizes should not unify
    let vec3 = Type::Vector(Box::new(Type::I32), zetac::middle::types::ArraySize::Literal(8));
    assert!(sub.unify(&vec1, &vec3).is_err());
    
    // Different element types should not unify
    let vec4 = Type::Vector(Box::new(Type::F32), zetac::middle::types::ArraySize::Literal(4));
    assert!(sub.unify(&vec1, &vec4).is_err());
    
    // Vector with type variable
    let tvar = Type::Variable(TypeVar::fresh());
    let vec5 = Type::Vector(Box::new(tvar.clone()), zetac::middle::types::ArraySize::Literal(4));
    let vec6 = Type::Vector(Box::new(Type::I32), zetac::middle::types::ArraySize::Literal(4));
    
    assert!(sub.unify(&vec5, &vec6).is_ok());
    // After unification, tvar should be bound to i32
    let applied = sub.apply(&tvar);
    assert_eq!(applied, Type::I32);
}