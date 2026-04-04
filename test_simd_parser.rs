use src::frontend::parser::parser::parse_type;

fn test_simd_parsing() {
    println!("Testing SIMD type parsing...");
    
    // Test shorthand syntax
    let test_cases = vec![
        ("u64x8", "Vector<u64, 8>"),
        ("i32x16", "Vector<i32, 16>"),
        ("f32x4", "Vector<f32, 4>"),
        ("f64x8", "Vector<f64, 8>"),
        ("u8x32", "Vector<u8, 32>"),
        ("i16x8", "Vector<i16, 8>"),
    ];
    
    for (input, expected) in test_cases {
        match parse_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() {
                    if result == expected {
                        println!("✓ {} -> {}", input, result);
                    } else {
                        println!("✗ {} -> {} (expected: {})", input, result, expected);
                    }
                } else {
                    println!("✗ {}: didn't consume all input, remaining: '{}'", input, remaining);
                }
            }
            Err(e) => {
                println!("✗ {}: parse error: {:?}", input, e);
            }
        }
    }
    
    // Test Vector<T, N> syntax
    println!("\nTesting Vector<T, N> syntax...");
    let test_cases = vec![
        ("Vector<u64, 8>", "Vector<u64, 8>"),
        ("Vector<i32, 16>", "Vector<i32, 16>"),
        ("Vector<f32, 4>", "Vector<f32, 4>"),
    ];
    
    for (input, expected) in test_cases {
        match parse_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() {
                    if result == expected {
                        println!("✓ {} -> {}", input, result);
                    } else {
                        println!("✗ {} -> {} (expected: {})", input, result, expected);
                    }
                } else {
                    println!("✗ {}: didn't consume all input, remaining: '{}'", input, remaining);
                }
            }
            Err(e) => {
                println!("✗ {}: parse error: {:?}", input, e);
            }
        }
    }
    
    // Test with references
    println!("\nTesting SIMD types with references...");
    let test_cases = vec![
        ("&u64x8", "&Vector<u64, 8>"),
        ("&mut f32x4", "&mut Vector<f32, 4>"),
    ];
    
    for (input, expected) in test_cases {
        match parse_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() {
                    if result == expected {
                        println!("✓ {} -> {}", input, result);
                    } else {
                        println!("✗ {} -> {} (expected: {})", input, result, expected);
                    }
                } else {
                    println!("✗ {}: didn't consume all input, remaining: '{}'", input, remaining);
                }
            }
            Err(e) => {
                println!("✗ {}: parse error: {:?}", input, e);
            }
        }
    }
}

fn main() {
    test_simd_parsing();
}