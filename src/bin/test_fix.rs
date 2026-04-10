use zetac::frontend::parser::parser::parse_array_type;

fn test_parse_array_type(input: &str, expected: &str) {
    println!("Testing: '{}'", input);
    match parse_array_type(input) {
        Ok((remaining, result)) => {
            if !remaining.is_empty() {
                println!("  WARNING: Did not consume all input: '{}'", remaining);
            }
            if result == expected {
                println!("  ✓ PASS: Got '{}'", result);
            } else {
                println!("  ✗ FAIL: Expected '{}', got '{}'", expected, result);
            }
        }
        Err(e) => {
            println!("  ✗ ERROR: {:?}", e);
        }
    }
}

fn main() {
    println!("Testing array type parser fix for infinite recursion...\n");
    
    // Test cases that should work
    test_parse_array_type("[u64]", "[u64]");
    test_parse_array_type("[u64; 10]", "[u64; 10]");
    test_parse_array_type("[[u64]]", "[[u64]]");
    test_parse_array_type("[[u64; 4]; 3]", "[[u64; 4]; 3]");
    test_parse_array_type("[[[u64]]]", "[[[u64]]]");
    
    println!("\nTesting PrimeZeta style...");
    test_parse_array_type("[10]u64", "[u64; 10]");
    test_parse_array_type("[3][4]i64", "[[i64; 4]; 3]");
    
    println!("\nAll tests completed!");
}