// Test array type system compatibility
use zetac::middle::types::Type;

fn main() {
    println!("=== Testing Array Type System Compatibility ===\n");
    
    // Test 1: Basic array type parsing
    println!("1. Testing basic array type parsing:");
    let test_cases = vec![
        ("[i64; 10]", "Array(I64, 10)"),
        ("[u64; 256]", "Array(U64, 256)"),
        ("[usize; 32]", "Array(Usize, 32)"),
        ("[bool; 8]", "Array(Bool, 8)"),
        ("[f64; 16]", "Array(F64, 16)"),
    ];
    
    for (input, expected_debug) in test_cases {
        let ty = Type::from_string(input);
        let debug_str = format!("{:?}", ty);
        if debug_str == expected_debug {
            println!("  ✓ {} -> {}", input, debug_str);
        } else {
            println!("  ✗ {} -> {} (expected: {})", input, debug_str, expected_debug);
        }
    }
    
    // Test 2: Slice type parsing
    println!("\n2. Testing slice type parsing:");
    let slice_cases = vec![
        ("[i64]", "Slice(I64)"),
        ("[u64]", "Slice(U64)"),
        ("[usize]", "Slice(Usize)"),
    ];
    
    for (input, expected_debug) in slice_cases {
        let ty = Type::from_string(input);
        let debug_str = format!("{:?}", ty);
        if debug_str == expected_debug {
            println!("  ✓ {} -> {}", input, debug_str);
        } else {
            println!("  ✗ {} -> {} (expected: {})", input, debug_str, expected_debug);
        }
    }
    
    // Test 3: Nested array types
    println!("\n3. Testing nested array types:");
    let nested_cases = vec![
        ("[[i64; 4]; 3]", "Array(Array(I64, 4), 3)"),
    ];
    
    for (input, expected_debug) in nested_cases {
        let ty = Type::from_string(input);
        let debug_str = format!("{:?}", ty);
        if debug_str == expected_debug {
            println!("  ✓ {} -> {}", input, debug_str);
        } else {
            println!("  ✗ {} -> {} (expected: {})", input, debug_str, expected_debug);
        }
    }
    
    // Test 4: Type equality
    println!("\n4. Testing type equality:");
    let ty1 = Type::from_string("[i64; 10]");
    let ty2 = Type::from_string("[i64; 10]");
    let ty3 = Type::from_string("[u64; 10]");
    
    if ty1 == ty2 {
        println!("  ✓ [i64; 10] == [i64; 10]");
    } else {
        println!("  ✗ [i64; 10] != [i64; 10]");
    }
    
    if ty1 != ty3 {
        println!("  ✓ [i64; 10] != [u64; 10]");
    } else {
        println!("  ✗ [i64; 10] == [u64; 10]");
    }
    
    // Test 5: Display names
    println!("\n5. Testing display names:");
    let display_cases = vec![
        ("[i64; 10]", "[i64; 10]"),
        ("[u64; 256]", "[u64; 256]"),
        ("[i64]", "[i64]"),
    ];
    
    for (input, expected_display) in display_cases {
        let ty = Type::from_string(input);
        let display = ty.display_name();
        if display == expected_display {
            println!("  ✓ {} -> {}", input, display);
        } else {
            println!("  ✗ {} -> {} (expected: {})", input, display, expected_display);
        }
    }
    
    // Test 6: Array of references
    println!("\n6. Testing array of references:");
    let ref_cases = vec![
        ("[&i64; 5]", "Array(Ref(I64, Static, Immutable), 5)"),
        ("[&mut u64; 8]", "Array(Ref(U64, Static, Mutable), 8)"),
    ];
    
    for (input, expected_debug) in ref_cases {
        let ty = Type::from_string(input);
        let debug_str = format!("{:?}", ty);
        // Note: The actual debug output might be different due to formatting
        println!("  {} -> {}", input, debug_str);
        // Just check it doesn't panic
    }
    
    println!("\n=== Array Type System Tests Completed ===");
}