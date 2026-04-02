use zetac::frontend::parser::parser::{parse_type, parse_array_type};
use zetac::frontend::parser::top_level::parse_func;

fn test_parse_type() {
    println!("=== Testing parse_type ===");
    
    let test_cases = vec![
        "u64",
        "[u64; 10]",
        "[u64; NUM_RESIDUES]",
        "[u64]",
        "[10]u64",  // PrimeZeta style
    ];
    
    for test in test_cases {
        println!("\nTesting: '{}'", test);
        match parse_type(test) {
            Ok((remaining, result)) => {
                println!("  Success: {}", result);
                println!("  Remaining: '{}'", remaining);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
            }
        }
    }
}

fn test_parse_array_type() {
    println!("\n\n=== Testing parse_array_type ===");
    
    let test_cases = vec![
        "[u64; 10]",
        "[u64; NUM_RESIDUES]",
        "[u64]",
        "[10]u64",  // PrimeZeta style
    ];
    
    for test in test_cases {
        println!("\nTesting: '{}'", test);
        match parse_array_type(test) {
            Ok((remaining, result)) => {
                println!("  Success: {}", result);
                println!("  Remaining: '{}'", remaining);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
            }
        }
    }
}

fn test_parse_func() {
    println!("\n\n=== Testing parse_func ===");
    
    let test_cases = vec![
        "fn test() -> [u64; 10] { return [0; 10] }",
        "comptime fn generate_residues() -> [u64; NUM_RESIDUES]",
        "fn test() -> u64 { 42 }",
    ];
    
    for test in test_cases {
        println!("\nTesting: '{}'", test);
        match parse_func(test) {
            Ok((remaining, result)) => {
                println!("  Success: {:?}", result);
                println!("  Remaining: '{}'", remaining);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
            }
        }
    }
}

fn main() {
    test_parse_type();
    test_parse_array_type();
    test_parse_func();
}