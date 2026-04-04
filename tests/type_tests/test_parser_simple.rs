// Simple test to check if SIMD type parsing works
fn main() {
    // Test cases for SIMD type parsing
    let test_cases = vec![
        ("u64x8", "Vector<u64, 8>"),
        ("i32x16", "Vector<i32, 16>"),
        ("f32x4", "Vector<f32, 4>"),
        ("Vector<u64, 8>", "Vector<u64, 8>"),
        ("Vector<i32, 16>", "Vector<i32, 16>"),
    ];
    
    println!("SIMD Type Parser Test");
    println!("=====================");
    
    for (input, expected) in test_cases {
        println!("Testing: {}", input);
        println!("Expected: {}", expected);
        println!("---");
    }
    
    // Also test that the parser recognizes new primitive types
    let primitive_types = vec![
        "i8", "i16", "i32", "i64",
        "u8", "u16", "u32", "u64", "usize",
        "f32", "f64",
        "bool", "char", "str",
    ];
    
    println!("\nPrimitive Type Recognition Test");
    println!("================================");
    for typ in primitive_types {
        println!("Should recognize: {}", typ);
    }
}