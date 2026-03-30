use zetac::frontend::parser::parser::parse_type;

fn test_type_parsing(type_str: &str) {
    println!("Testing type parsing: '{}'", type_str);
    match parse_type(type_str) {
        Ok((remaining, parsed_type)) => {
            if !remaining.is_empty() {
                println!("  ERROR: Unparsed input: '{}'", remaining);
            } else {
                println!("  Success: '{}'", parsed_type);
                println!("  Debug: {:?}", parsed_type);
            }
        }
        Err(e) => {
            println!("  ERROR: Parse error: {:?}", e);
        }
    }
    println!();
}

fn main() {
    println!("Testing type variable parsing...\n");

    // Test type variables
    test_type_parsing("T");
    test_type_parsing("U");
    test_type_parsing("Result");
    test_type_parsing("Result<T>");
    test_type_parsing("Result<T, E>");
    test_type_parsing("Vec<T>");
    test_type_parsing("Option<T>");
    
    // Test with concrete types for comparison
    test_type_parsing("i32");
    test_type_parsing("String");
    test_type_parsing("Vec<i32>");
    test_type_parsing("Option<bool>");
    
    // Test complex types
    test_type_parsing("&T");
    test_type_parsing("&mut T");
    test_type_parsing("Vec<&mut T>");
    
    // Test the lt() syntax
    test_type_parsing("lt(Result, i64)");
    test_type_parsing("lt(Vec, T)");
    test_type_parsing("lt(Option, bool)");
}