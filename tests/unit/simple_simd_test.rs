#[test]
fn test_simd_type_basics() {
    use zetac::middle::types::{Type, ArraySize};
    
    // Create a simple vector type
    let vec_type = Type::Vector(Box::new(Type::I32), ArraySize::Literal(4));
    
    // Test basic properties
    assert!(vec_type.is_vector());
    assert_eq!(vec_type.display_name(), "Vector<i32, 4>");
    
    println!("SIMD type test passed!");
}