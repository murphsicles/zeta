use zetac::middle::types::Type;

fn main() {
    println!("Testing improved Type::from_string() implementation:\n");

    // Test basic types
    println!("1. i32: {:?}", Type::from_string("i32"));
    println!("2. bool: {:?}", Type::from_string("bool"));
    println!("3. str: {:?}", Type::from_string("str"));

    // Test reference types
    println!("\n4. &str: {:?}", Type::from_string("&str"));
    println!("5. &mut i64: {:?}", Type::from_string("&mut i64"));

    // Test array and slice types
    println!("\n6. [i32; 10]: {:?}", Type::from_string("[i32; 10]"));
    println!("7. [bool]: {:?}", Type::from_string("[bool]"));

    // Test tuple types
    println!("\n8. (): {:?}", Type::from_string("()"));
    println!("9. (i32, bool): {:?}", Type::from_string("(i32, bool)"));
    println!(
        "10. (i64, &str, bool): {:?}",
        Type::from_string("(i64, &str, bool)")
    );

    // Test single generic argument
    println!("\n11. Vec<i32>: {:?}", Type::from_string("Vec<i32>"));

    // Test multiple generic arguments
    println!(
        "12. Result<i32, String>: {:?}",
        Type::from_string("Result<i32, String>")
    );

    // Test nested generics
    println!(
        "13. Vec<Vec<i32>>: {:?}",
        Type::from_string("Vec<Vec<i32>>")
    );
    println!(
        "14. Option<Vec<bool>>: {:?}",
        Type::from_string("Option<Vec<bool>>")
    );

    // Test with spaces
    println!(
        "15. Result < i32, String >: {:?}",
        Type::from_string("Result < i32, String >")
    );

    // Test Zeta's lt() syntax
    println!(
        "16. lt(Result, i64): {:?}",
        Type::from_string("lt(Result, i64)")
    );

    // Test complex nested case
    println!(
        "17. Vec<Result<i32, String>>: {:?}",
        Type::from_string("Vec<Result<i32, String>>")
    );
}
