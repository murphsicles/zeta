// Direct test of the parser's array type parsing
use zetac::frontend::parser::parser::parse_array_type;

fn main() {
    println!("=== Testing Array Type Parser ===");
    
    // Test cases: (input, expected_output)
    let test_cases = vec![
        ("[i64; 10]", "[i64; 10]"),
        ("[10]i64", "[i64; 10]"),  // PrimeZeta style should convert to Zeta style
        ("[u64; 256]", "[u64; 256]"),
        ("[256]u64", "[u64; 256]"),
        ("[usize; 32]", "[usize; 32]"),
        ("[32]usize", "[usize; 32]"),
        ("[[i64; 4]; 3]", "[[i64; 4]; 3]"),
        ("[3][4]i64", "[[i64; 4]; 3]"),
    ];
    
    let mut all_passed = true;
    
    for (input, expected) in test_cases {
        println!("\nTesting: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                if remaining.is_empty() {
                    if result == expected {
                        println!("  ✓ PASS: Got '{}'", result);
                    } else {
                        println!("  ✗ FAIL: Expected '{}', got '{}'", expected, result);
                        all_passed = false;
                    }
                } else {
                    println!("  ✗ FAIL: Did not consume all input. Remaining: '{}'", remaining);
                    all_passed = false;
                }
            }
            Err(e) => {
                println!("  ✗ FAIL: Parse error: {:?}", e);
                all_passed = false;
            }
        }
    }
    
    println!("\n=== Summary ===");
    if all_passed {
        println!("✓ All tests passed!");
    } else {
        println!("✗ Some tests failed");
        std::process::exit(1);
    }
}