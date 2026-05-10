use zetac::frontend::parser::parser::{parse_type, parse_array_type};
use zetac::frontend::parser::top_level::parse_zeta;

#[test]
fn test_parse_type() {
    println!("=== Testing parse_type ===");
    
    let test_cases = vec![
        ("u64", true),
        ("[u64; 10]", true),
        ("[u64; NUM_RESIDUES]", true),
        ("[u64]", true),
        ("[10]u64", true),  // PrimeZeta style
    ];
    
    for (test, should_succeed) in test_cases {
        println!("\nTesting: '{}'", test);
        match parse_type(test) {
            Ok((remaining, result)) => {
                println!("  Success: {}", result);
                println!("  Remaining: '{}'", remaining);
                assert!(should_succeed, "Expected failure but got success for: {}", test);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
                assert!(!should_succeed, "Expected success but got failure for: {}", test);
            }
        }
    }
}

#[test]
fn test_parse_array_type() {
    println!("\n\n=== Testing parse_array_type ===");
    
    let test_cases = vec![
        ("[u64; 10]", true),
        ("[u64; NUM_RESIDUES]", true),
        ("[u64]", true),
        ("[10]u64", true),  // PrimeZeta style
    ];
    
    for (test, should_succeed) in test_cases {
        println!("\nTesting: '{}'", test);
        match parse_array_type(test) {
            Ok((remaining, result)) => {
                println!("  Success: {}", result);
                println!("  Remaining: '{}'", remaining);
                assert!(should_succeed, "Expected failure but got success for: {}", test);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
                assert!(!should_succeed, "Expected success but got failure for: {}", test);
            }
        }
    }
}

#[test]
fn test_parse_func() {
    println!("\n\n=== Testing parse_func ===");
    
    let test_cases = vec![
        ("fn test() -> [u64; 10] { return [0; 10] }", true),
        ("comptime fn generate_residues() -> [u64; NUM_RESIDUES] { return [0; NUM_RESIDUES] }", true),
        ("fn test() -> u64 { 42 }", true),
    ];
    
    for (test, should_succeed) in test_cases {
        println!("\nTesting: '{}'", test);
        match parse_zeta(test) {
            Ok((remaining, result)) => {
                println!("  Success: parsed {} AST nodes", result.len());
                println!("  Remaining: '{}'", remaining);
                assert!(should_succeed, "Expected failure but got success for: {}", test);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
                assert!(!should_succeed, "Expected success but got failure for: {}", test);
            }
        }
    }
}