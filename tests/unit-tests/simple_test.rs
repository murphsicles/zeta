use nom::IResult;

// Minimal version of parse_type to test
fn parse_type_minimal(input: &str) -> IResult<&str, String> {
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::combinator::map;
    
    alt((
        tag("u64").map(|_| "u64".to_string()),
        tag("i64").map(|_| "i64".to_string()),
        // parse_array_type would be here
    ))(input)
}

fn parse_array_type_minimal(input: &str) -> IResult<&str, String> {
    use nom::bytes::complete::tag;
    use nom::sequence::{delimited, preceded};
    use nom::combinator::opt;
    
    // Zeta style: [T; N]
    let (input, _) = tag("[")(input)?;
    let (input, elem_type) = parse_type_minimal(input)?; // Recursive call!
    let (input, _) = tag(";")(input)?;
    let (input, size) = nom::character::complete::digit1(input)?;
    let (input, _) = tag("]")(input)?;
    
    Ok((input, format!("[{}; {}]", elem_type, size)))
}

fn main() {
    println!("Testing minimal parser:");
    
    // Test 1: parse u64 directly
    match parse_type_minimal("u64") {
        Ok((rem, result)) => println!("parse_type_minimal('u64') = {:?}, rem: {:?}", result, rem),
        Err(e) => println!("parse_type_minimal('u64') failed: {:?}", e),
    }
    
    // Test 2: parse array type
    match parse_array_type_minimal("[u64; 10]") {
        Ok((rem, result)) => println!("parse_array_type_minimal('[u64; 10]') = {:?}, rem: {:?}", result, rem),
        Err(e) => println!("parse_array_type_minimal('[u64; 10]') failed: {:?}", e),
    }
    
    // The issue might be that when parse_array_type is called from parse_type,
    // and parse_array_type calls parse_type recursively, there could be
    // infinite recursion or wrong alternative matching.
    
    // Actually, looking at the real parse_type function, it has parse_array_type
    // as one of the alternatives. So when parse_array_type calls parse_type
    // for the element type, parse_type will try parse_array_type again!
    // This could cause issues if the input starts with '['.
    
    // For example: parsing "[u64; 10]"
    // 1. parse_type is called
    // 2. It tries alternatives, reaches parse_array_type
    // 3. parse_array_type parses '['
    // 4. parse_array_type calls parse_type for "u64; 10]"
    // 5. parse_type tries alternatives, reaches parse_array_type again
    // 6. parse_array_type tries to parse '[' but finds 'u64' -> fails
    // 7. parse_type tries next alternative (tag("u64")) -> succeeds
    // 8. Back in parse_array_type, continues parsing...
    
    // This should work! But maybe there's an issue with whitespace?
    
    println!("\nActual issue might be:");
    println!("1. Whitespace handling in ws() wrapper");
    println!("2. Order of alternatives in parse_type");
    println!("3. The dual syntax support in parse_array_type");
}