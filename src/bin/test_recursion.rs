use zetac::frontend::parser::parser::parse_array_type;

fn main() {
    println!("Testing nested array parsing (should cause infinite recursion)...");
    
    let test_cases = vec![
        "[[u64]]",
        "[[u64; 4]; 3]",
        "[[[u64]]]",
    ];
    
    for input in test_cases {
        println!("\nTesting: '{}'", input);
        match parse_array_type(input) {
            Ok((remaining, result)) => {
                println!("  Success: '{}' -> '{}'", result, remaining);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
            }
        }
    }
}