// Simple test to check if range parsing works
mod test_parser {
    use nom::IResult;
    
    // Simplified parser for testing
    fn parse_range(input: &str) -> IResult<&str, &str> {
        if input.starts_with("..") {
            Ok((&input[2..], ".."))
        } else if input.starts_with("..=") {
            Ok((&input[3..], "..="))
        } else {
            Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
        }
    }
    
    pub fn test() {
        println!("Testing range parser...");
        
        // Test ..
        match parse_range("..10") {
            Ok((remaining, op)) => println!("  Found operator '{}', remaining: '{}'", op, remaining),
            Err(e) => println!("  Error: {:?}", e),
        }
        
        // Test ..=
        match parse_range("..=10") {
            Ok((remaining, op)) => println!("  Found operator '{}', remaining: '{}'", op, remaining),
            Err(e) => println!("  Error: {:?}", e),
        }
        
        // Test not a range
        match parse_range(".10") {
            Ok((remaining, op)) => println!("  Found operator '{}', remaining: '{}'", op, remaining),
            Err(_) => println!("  Correctly rejected non-range"),
        }
    }
}

fn main() {
    test_parser::test();
}