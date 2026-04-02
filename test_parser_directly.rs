// Direct test of the parser module
mod test_parser {
    use nom::IResult;
    use nom::Parser;
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_until, take_while};
    use nom::character::complete::{alpha1, multispace1, satisfy};
    use nom::combinator::{map, opt, recognize, value, verify};
    use nom::multi::{many0, many1, separated_list0, separated_list1};
    use nom::sequence::{delimited, pair, preceded, terminated};
    
    pub fn line_comment(input: &str) -> IResult<&str, ()> {
        value((), pair(tag("//"), take_while(|c| c != '\n' && c != '\r'))).parse(input)
    }
    
    pub fn block_comment(input: &str) -> IResult<&str, ()> {
        value((), delimited(tag("/*"), take_until("*/"), tag("*/"))).parse(input)
    }
    
    pub fn skip_ws_and_comments0(input: &str) -> IResult<&str, ()> {
        value(
            (),
            many0(alt((value((), multispace1), line_comment, block_comment))),
        )
        .parse(input)
    }
    
    /// Parse a single attribute (e.g., #[test] or #[derive(Clone, Debug)])
    pub fn parse_attribute(input: &str) -> IResult<&str, String> {
        let (input, _) = tag("#[")(input)?;
        let (input, content) = take_until("]")(input)?;
        let (input, _) = tag("]")(input)?;
    
        // Trim whitespace from the content
        let trimmed_content = content.trim();
        Ok((input, trimmed_content.to_string()))
    }
    
    /// Parse zero or more attributes
    pub fn parse_attributes(input: &str) -> IResult<&str, Vec<String>> {
        let mut attributes = Vec::new();
        let mut current_input = input;
    
        loop {
            // Skip whitespace and comments before checking for attribute
            let (input_after_ws, _) = skip_ws_and_comments0(current_input)?;
    
            // Check if we have an attribute
            if input_after_ws.starts_with("#[") {
                let (input_after_attr, attr) = parse_attribute(input_after_ws)?;
                attributes.push(attr);
                current_input = input_after_attr;
            } else {
                // No more attributes
                break;
            }
        }
    
        Ok((current_input, attributes))
    }
}

fn main() {
    println!("=== Testing #[ai_opt] Attribute Parsing ===\n");
    
    // Test cases
    let test_cases = vec![
        ("#[ai_opt]", "ai_opt"),
        ("#[ai_opt] fn foo() {}", "ai_opt"),
        ("#[derive(Clone, Debug)]", "derive(Clone, Debug)"),
        ("#[cfg(test)] #[allow(unused)]", "cfg(test)"),
        ("#![feature(special)]", "!feature(special)"), // Note: inner attribute
    ];
    
    for (i, (input, expected)) in test_cases.iter().enumerate() {
        println!("Test {}: Parsing '{}'", i + 1, input);
        match test_parser::parse_attribute(input) {
            Ok((remaining, attr)) => {
                if attr == *expected {
                    println!("  ✅ Parsed correctly: '{}'", attr);
                } else {
                    println!("  ❌ Expected '{}', got '{}'", expected, attr);
                }
                println!("  Remaining: '{}'", remaining);
            }
            Err(e) => {
                println!("  ❌ Parse error: {:?}", e);
            }
        }
        println!();
    }
    
    // Test multiple attributes
    println!("=== Testing Multiple Attributes ===");
    let multi_attr = "#[cfg(test)] #[allow(unused)] #[ai_opt] fn foo() {}";
    match test_parser::parse_attributes(multi_attr) {
        Ok((remaining, attrs)) => {
            println!("✅ Parsed {} attributes:", attrs.len());
            for (i, attr) in attrs.iter().enumerate() {
                println!("  Attribute {}: '{}'", i, attr);
            }
            println!("Remaining: '{}'", remaining);
            
            // Check if #[ai_opt] is in the list
            if attrs.iter().any(|a| a == "ai_opt") {
                println!("✅ #[ai_opt] attribute is correctly parsed!");
            } else {
                println!("❌ #[ai_opt] attribute not found in parsed attributes");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {:?}", e);
        }
    }
}