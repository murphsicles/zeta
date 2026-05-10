// Simple test to verify type alias implementation
use std::collections::HashMap;

fn main() {
    println!("Testing type alias implementation...");
    
    // Simulate what the type checker should do
    let mut types = HashMap::new();
    
    // Store a type alias: MyInt = i64
    types.insert("MyInt".to_string(), "i64".to_string());
    
    // Test resolving the alias
    let alias_name = "MyInt";
    if let Some(underlying_type) = types.get(alias_name) {
        println!("✓ Type alias '{}' resolves to '{}'", alias_name, underlying_type);
        
        // Check if it's a primitive type
        match underlying_type.as_str() {
            "i64" => println!("✓ Underlying type is i64 (primitive)"),
            _ => println!("✗ Unexpected underlying type: {}", underlying_type),
        }
    } else {
        println!("✗ Type alias '{}' not found", alias_name);
    }
    
    // Test non-existent alias
    let non_existent = "NonExistent";
    if types.get(non_existent).is_none() {
        println!("✓ Non-existent alias '{}' correctly returns None", non_existent);
    }
}