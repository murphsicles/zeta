use zetac::frontend::parser::parser::parse_type;

fn main() {
    let test_cases = vec![
        "string",
        "string[identity:read]",
        "string[identity:read+write]",
        "i64",
        "bool",
    ];
    
    for test in test_cases {
        println!("Testing: '{}'", test);
        match parse_type(test) {
            Ok((remaining, result)) => {
                println!("  Success: '{}', Remaining: '{}'", result, remaining);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
            }
        }
    }
}