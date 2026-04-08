// Simple test for array syntax parsing
use zetac::frontend::parser::parser::parse_type;

fn test_array_type(input: &str, expected: &str) {
    println!("Testing: '{}'", input);
    match parse_type(input) {
        Ok((remaining, result)) => {
            if remaining.is_empty() {
                println!("  ✅ Parsed successfully!");
                println!("  Result: '{}'", result);
                if result == expected {
                    println!("  ✅ Matches expected: '{}'", expected);
                } else {
                    println!("  ❌ Does NOT match expected: '{}'", expected);
                }
            } else {
                println!("  ⚠️  Did not consume all input: '{}'", remaining);
                println!("  Result: '{}'", result);
            }
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    println!();
}

fn main() {
    println!("=== Testing Array Type Syntax ===\n");
    
    // Test Zeta style: [T; N]
    println!("1. Testing Zeta style [T; N]:");
    test_array_type("[i64; 10]", "[i64; 10]");
    test_array_type("[u64; 256]", "[u64; 256]");
    test_array_type("[bool; 8]", "[bool; 8]");
    test_array_type("[f64; 16]", "[f64; 16]");
    
    // Test PrimeZeta style: [N]T
    println!("\n2. Testing PrimeZeta style [N]T:");
    test_array_type("[10]i64", "[i64; 10]");
    test_array_type("[256]u64", "[u64; 256]");
    test_array_type("[8]bool", "[bool; 8]");
    test_array_type("[16]f64", "[f64; 16]");
    
    // Test with identifier sizes
    println!("\n3. Testing with identifier sizes:");
    test_array_type("[NUM_RESIDUES]u64", "[u64; NUM_RESIDUES]");
    test_array_type("[BUFFER_SIZE]u8", "[u8; BUFFER_SIZE]");
    test_array_type("[COUNT]usize", "[usize; COUNT]");
    
    // Test slices: [T]
    println!("\n4. Testing slice types [T]:");
    test_array_type("[i64]", "[i64]");
    test_array_type("[u64]", "[u64]");
    test_array_type("[bool]", "[bool]");
    
    // Test nested arrays
    println!("\n5. Testing nested arrays:");
    test_array_type("[[i64; 4]; 3]", "[[i64; 4]; 3]");
    test_array_type("[[u64; 2]; 5]", "[[u64; 2]; 5]");
    
    // Test function return types with arrays
    println!("\n6. Testing function with array return type:");
    // Note: This would need parse_fn_type, but let's test parse_type on just the return type
    test_array_type("[u64; NUM_RESIDUES]", "[u64; NUM_RESIDUES]");
    
    println!("=== Test Complete ===");
}