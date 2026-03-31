use zetac::middle::types::Type;

fn main() {
    println!("Testing current Type::from_string() implementation:");

    // Test basic types
    println!("1. i32: {:?}", Type::from_string("i32"));
    println!("2. bool: {:?}", Type::from_string("bool"));

    // Test single generic argument
    println!("3. Vec<i32>: {:?}", Type::from_string("Vec<i32>"));

    // Test multiple generic arguments
    println!(
        "4. Result<i32, String>: {:?}",
        Type::from_string("Result<i32, String>")
    );

    // Test nested generics
    println!("5. Vec<Vec<i32>>: {:?}", Type::from_string("Vec<Vec<i32>>"));

    // Test complex generic with tuple
    println!(
        "6. HashMap<(String, String), Vec<AstNode>>: {:?}",
        Type::from_string("HashMap<(String, String), Vec<AstNode>>")
    );

    // Test trait object
    println!(
        "7. Box<dyn std::error::Error>: {:?}",
        Type::from_string("Box<dyn std::error::Error>")
    );

    // Test pointer types
    println!("8. *const c_char: {:?}", Type::from_string("*const c_char"));
    println!("9. *mut c_void: {:?}", Type::from_string("*mut c_void"));

    // Test with module paths
    println!(
        "10. std::collections::HashMap<String, i32>: {:?}",
        Type::from_string("std::collections::HashMap<String, i32>")
    );
}
