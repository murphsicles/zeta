use zetac::frontend::parser::parser::parse_type;

fn main() {
    let test_cases = vec![
        "u64",
        "[u64]",
        "[u64; 5]",
        "&u64",
        "&mut u64",
        "(u64, u64)",
        "fn(u64) -> u64",
    ];

    for test in test_cases {
        println!("Testing: '{}'", test);
        match parse_type(test) {
            Ok((remaining, result)) => {
                println!("  Success: '{}' -> '{}'", result, remaining);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
            }
        }
        println!();
    }
}
