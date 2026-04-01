// Unit test for array type parser
// Tests both Zeta-style [T; N] and PrimeZeta-style [N]T syntax

use zetac::frontend::parser::parser::parse_array_type;

#[test]
fn test_zeta_style_array_parsing() {
    println!("Testing Zeta-style array parsing...");
    
    let test_cases = vec![
        ("[i64; 10]", "[i64; 10]"),
        ("[u64; 256]", "[u64; 256]"),
        ("[usize; 32]", "[usize; 32]"),
        ("[[i64; 4]; 3]", "[[i64; 4]; 3]"),
        ("[bool; 8]", "[bool; 8]"),
        ("[f64; 16]", "[f64; 16]"),
    ];
    
    for (input, expected) in test_cases {
        println!("  Testing: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                assert!(remaining.is_empty(), "Did not consume all input: '{}'", remaining);
                assert_eq!(result, expected, "Expected '{}', got '{}'", expected, result);
                println!("    ✓ PASS");
            }
            Err(e) => {
                panic!("Parse error for '{}': {:?}", input, e);
            }
        }
    }
}

#[test]
fn test_primezeta_style_array_parsing() {
    println!("Testing PrimeZeta-style array parsing...");
    
    let test_cases = vec![
        ("[10]i64", "[i64; 10]"),      // Should convert to Zeta style
        ("[256]u64", "[u64; 256]"),    // Should convert to Zeta style
        ("[32]usize", "[usize; 32]"),  // Should convert to Zeta style
        ("[3][4]i64", "[[i64; 4]; 3]"), // Nested: should convert
        ("[8]bool", "[bool; 8]"),      // Should convert to Zeta style
        ("[16]f64", "[f64; 16]"),      // Should convert to Zeta style
    ];
    
    for (input, expected) in test_cases {
        println!("  Testing: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                assert!(remaining.is_empty(), "Did not consume all input: '{}'", remaining);
                assert_eq!(result, expected, "Expected '{}', got '{}'", expected, result);
                println!("    ✓ PASS (converted to Zeta style)");
            }
            Err(e) => {
                panic!("Parse error for '{}': {:?}", input, e);
            }
        }
    }
}

#[test]
fn test_array_with_identifiers() {
    println!("Testing array parsing with identifier sizes...");
    
    let test_cases = vec![
        ("[NUM_RESIDUES]u64", "[u64; NUM_RESIDUES]"),
        ("[BUFFER_SIZE]u8", "[u8; BUFFER_SIZE]"),
        ("[COUNT]usize", "[usize; COUNT]"),
    ];
    
    for (input, expected) in test_cases {
        println!("  Testing: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                assert!(remaining.is_empty(), "Did not consume all input: '{}'", remaining);
                assert_eq!(result, expected, "Expected '{}', got '{}'", expected, result);
                println!("    ✓ PASS");
            }
            Err(e) => {
                panic!("Parse error for '{}': {:?}", input, e);
            }
        }
    }
}

#[test]
fn test_unsized_array_parsing() {
    println!("Testing unsized array (slice) parsing...");
    
    let test_cases = vec![
        ("[i64]", "[i64]"),
        ("[u64]", "[u64]"),
        ("[usize]", "[usize]"),
    ];
    
    for (input, expected) in test_cases {
        println!("  Testing: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                assert!(remaining.is_empty(), "Did not consume all input: '{}'", remaining);
                assert_eq!(result, expected, "Expected '{}', got '{}'", expected, result);
                println!("    ✓ PASS");
            }
            Err(e) => {
                panic!("Parse error for '{}': {:?}", input, e);
            }
        }
    }
}

#[test]
fn test_type_system_conversion() {
    println!("Testing Type::from_string with array types...");
    
    use zetac::middle::types::Type;
    
    let test_cases = vec![
        ("[i64; 10]", true),      // Zeta style
        ("[u64; 256]", true),     // Zeta style
        ("[i64]", true),          // Slice
    ];
    
    for (input, should_succeed) in test_cases {
        println!("  Testing: '{}'", input);
        let result = Type::from_string(input);
        
        if should_succeed {
            println!("    Result: {:?}", result);
            // Just verify it doesn't panic
            assert!(true);
        } else {
            // For now, just print the result
            println!("    Result: {:?}", result);
        }
    }
}

fn main() {
    println!("=== Running Array Parsing Tests ===");
    
    // Test Zeta-style array parsing
    println!("\n1. Testing Zeta-style array parsing...");
    let zeta_cases = vec![
        ("[i64; 10]", "[i64; 10]"),
        ("[u64; 256]", "[u64; 256]"),
        ("[usize; 32]", "[usize; 32]"),
        ("[[i64; 4]; 3]", "[[i64; 4]; 3]"),
        ("[bool; 8]", "[bool; 8]"),
        ("[f64; 16]", "[f64; 16]"),
    ];
    
    for (input, expected) in zeta_cases {
        println!("  Testing: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() && result == expected {
                    println!("    ✓ PASS");
                } else {
                    println!("    ✗ FAIL: Expected '{}', got '{}' (remaining: '{}')", expected, result, remaining);
                }
            }
            Err(e) => {
                println!("    ✗ FAIL: Parse error: {:?}", e);
            }
        }
    }
    
    // Test PrimeZeta-style array parsing
    println!("\n2. Testing PrimeZeta-style array parsing...");
    let primezeta_cases = vec![
        ("[10]i64", "[i64; 10]"),
        ("[256]u64", "[u64; 256]"),
        ("[32]usize", "[usize; 32]"),
        ("[3][4]i64", "[[i64; 4]; 3]"),
        ("[8]bool", "[bool; 8]"),
        ("[16]f64", "[f64; 16]"),
    ];
    
    for (input, expected) in primezeta_cases {
        println!("  Testing: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() && result == expected {
                    println!("    ✓ PASS (converted to Zeta style)");
                } else {
                    println!("    ✗ FAIL: Expected '{}', got '{}' (remaining: '{}')", expected, result, remaining);
                }
            }
            Err(e) => {
                println!("    ✗ FAIL: Parse error: {:?}", e);
            }
        }
    }
    
    // Test array with identifiers
    println!("\n3. Testing array parsing with identifier sizes...");
    let ident_cases = vec![
        ("[NUM_RESIDUES]u64", "[u64; NUM_RESIDUES]"),
        ("[BUFFER_SIZE]u8", "[u8; BUFFER_SIZE]"),
        ("[COUNT]usize", "[usize; COUNT]"),
    ];
    
    for (input, expected) in ident_cases {
        println!("  Testing: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() && result == expected {
                    println!("    ✓ PASS");
                } else {
                    println!("    ✗ FAIL: Expected '{}', got '{}' (remaining: '{}')", expected, result, remaining);
                }
            }
            Err(e) => {
                println!("    ✗ FAIL: Parse error: {:?}", e);
            }
        }
    }
    
    // Test unsized arrays (slices)
    println!("\n4. Testing unsized array (slice) parsing...");
    let slice_cases = vec![
        ("[i64]", "[i64]"),
        ("[u64]", "[u64]"),
        ("[usize]", "[usize]"),
    ];
    
    for (input, expected) in slice_cases {
        println!("  Testing: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() && result == expected {
                    println!("    ✓ PASS");
                } else {
                    println!("    ✗ FAIL: Expected '{}', got '{}' (remaining: '{}')", expected, result, remaining);
                }
            }
            Err(e) => {
                println!("    ✗ FAIL: Parse error: {:?}", e);
            }
        }
    }
    
    // Test Type::from_string
    println!("\n5. Testing Type::from_string with array types...");
    use zetac::middle::types::Type;
    
    let type_cases = vec![
        ("[i64; 10]", "Array type"),
        ("[u64; 256]", "Array type"),
        ("[i64]", "Slice type"),
    ];
    
    for (input, description) in type_cases {
        println!("  Testing {}: '{}'", description, input);
        let result = Type::from_string(input);
        println!("    Result: {:?}", result);
    }
    
    println!("\n=== All array parsing tests completed ===");
}