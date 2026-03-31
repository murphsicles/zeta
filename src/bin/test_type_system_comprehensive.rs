use zetac::middle::types::Type;

fn test_complex_types() {
    println!("=== Testing Complex Type Parsing ===");

    // Test 1: Nested generics
    let nested = Type::from_string("Vec<Vec<Vec<i32>>>");
    println!("1. Vec<Vec<Vec<i32>>>: {}", nested.display_name());
    assert!(matches!(nested, Type::Named(_, _)));

    // Test 2: Multiple generic arguments with complex types
    let complex = Type::from_string("Result<Vec<String>, Box<dyn Error>>");
    println!(
        "2. Result<Vec<String>, Box<dyn Error>>: {}",
        complex.display_name()
    );

    // Test 3: Tuple in generic
    let tuple_in_generic = Type::from_string("HashMap<(String, i32), bool>");
    println!(
        "3. HashMap<(String, i32), bool>: {}",
        tuple_in_generic.display_name()
    );

    // Test 4: Pointer types
    let const_ptr = Type::from_string("*const i32");
    println!("4. *const i32: {}", const_ptr.display_name());
    assert!(matches!(const_ptr, Type::Ptr(_, _)));

    let mut_ptr = Type::from_string("*mut f64");
    println!("5. *mut f64: {}", mut_ptr.display_name());
    assert!(matches!(mut_ptr, Type::Ptr(_, _)));

    // Test 5: Trait objects
    let trait_obj = Type::from_string("dyn std::fmt::Debug");
    println!("6. dyn std::fmt::Debug: {}", trait_obj.display_name());
    assert!(matches!(trait_obj, Type::TraitObject(_)));

    // Test 6: Trait object in container
    let box_trait = Type::from_string("Box<dyn std::error::Error + Send + Sync>");
    println!(
        "7. Box<dyn std::error::Error + Send + Sync>: {}",
        box_trait.display_name()
    );

    // Test 7: Complex real-world example from v0.5.0
    let real_world = Type::from_string("HashMap<(String, String), Vec<AstNode>>");
    println!(
        "8. HashMap<(String, String), Vec<AstNode>>: {}",
        real_world.display_name()
    );

    // Test 8: Reference types
    let ref_type = Type::from_string("&mut String");
    println!("9. &mut String: {}", ref_type.display_name());
    assert!(matches!(ref_type, Type::Ref(_, _, _)));

    let const_ref = Type::from_string("&i32");
    println!("10. &i32: {}", const_ref.display_name());
    assert!(matches!(const_ref, Type::Ref(_, _, _)));

    println!("\n=== All tests passed! ===");
}

fn main() {
    test_complex_types();
}
