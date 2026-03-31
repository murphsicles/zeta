use zetac::frontend::parser::parser::parse_type;

fn main() {
    println!("Testing type variable resolution in resolver...\n");

    // Note: parse_type_string is a private method, so we can't test it directly
    // This test needs to be updated to use public APIs or the method needs to be made public

    println!("Test skipped: parse_type_string is a private method");
    println!("To run this test, make parse_type_string public in new_resolver.rs");

    // Test parser output for type variables
    println!("\nTest: Parser output for type variables");
    match parse_type("T") {
        Ok((remaining, parsed)) => {
            println!("  Parser output: '{}'", parsed);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => println!("  Parser error: {:?}", e),
    }

    match parse_type("Vec<T>") {
        Ok((remaining, parsed)) => {
            println!("  Parser output for Vec<T>: '{}'", parsed);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => println!("  Parser error: {:?}", e),
    }
}
