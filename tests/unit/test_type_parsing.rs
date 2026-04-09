use zetac::middle::types::Type;

fn test_type_from_string() {
    println!("Testing Type::from_string() implementation:");

    // Test basic types
    println!("1. i32: {:?}", Type::from_string("i32"));
    println!("2. bool: {:?}", Type::from_string("bool"));

    // Test single generic argument
    println!("3. Vec<i32>: {:?}", Type::from_string("Vec<i32>"));

    // Test multiple generic arguments (THIS WILL FAIL)
    println!(
        "4. Result<i32, String>: {:?}",
        Type::from_string("Result<i32, String>")
    );

    // Test nested generics (THIS WILL FAIL)
    println!("5. Vec<Vec<i32>>: {:?}", Type::from_string("Vec<Vec<i32>>"));

    // Test with spaces
    println!(
        "6. Result < i32, String >: {:?}",
        Type::from_string("Result < i32, String >")
    );
}

fn main() {
    test_type_from_string();
}
