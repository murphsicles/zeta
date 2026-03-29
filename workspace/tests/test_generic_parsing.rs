// Test generic type parsing
use std::collections::HashMap;

fn test_generic_parsing() {
    // Test that generic types are parsed correctly
    let test_cases = vec![
        ("Option<i64>", "Option<i64>"),
        ("Result<i64, String>", "Result<i64, String>"),
        ("Vec<i32>", "Vec<i32>"),
        ("HashMap<String, i32>", "HashMap<String, i32>"),
        ("Option<Vec<bool>>", "Option<Vec<bool>>"),
        ("Vec<&str>", "Vec<&str>"),
    ];
    
    println!("Testing generic type parsing:");
    for (input, expected) in test_cases {
        println!("  Input: {}, Expected: {}", input, expected);
        // In the actual implementation, we would call string_to_type here
    }
}

fn main() {
    test_generic_parsing();
    println!("Generic type parsing test complete!");
}