use zetac::frontend::parser::parser::parse_array_type;

fn test_parse_array_type(input: &str) {
    println!("Testing: '{}'", input);
    match parse_array_type(input) {
        Ok((remaining, result)) => {
            println!("  Success: '{}' -> '{}'", result, remaining);
        }
        Err(e) => {
            println!("  Error: {:?}", e);
        }
    }
    println!();
}

fn main() {
    println!("Testing array type parser...\n");
    
    // Test PrimeZeta style: [N]T
    test_parse_array_type("[10]u64");
    test_parse_array_type("[NUM_RESIDUES]u64");
    test_parse_array_type("[MODULUS]u8");
    
    // Test Zeta style: [T; N]
    test_parse_array_type("[u64; 10]");
    test_parse_array_type("[u64; NUM_RESIDUES]");
    test_parse_array_type("[u8; MODULUS]");
    
    // Test edge cases
    test_parse_array_type("[u64]");  // Unsized
    test_parse_array_type("[10]");   // Invalid (missing type)
    test_parse_array_type("[u64;]"); // Invalid (missing size)
}