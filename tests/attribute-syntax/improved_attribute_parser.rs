// Improved attribute parser that handles nested brackets and inner attributes

fn parse_attribute_content(input: &str) -> Result<(String, &str), &'static str> {
    let mut depth = 0;
    let mut in_string = false;
    let mut escape_next = false;
    let mut content = String::new();
    
    for (i, c) in input.char_indices() {
        match c {
            '[' if !in_string => {
                depth += 1;
                content.push(c);
            }
            ']' if !in_string => {
                if depth == 0 {
                    // Found the closing bracket
                    return Ok((content, &input[i..]));
                }
                depth -= 1;
                content.push(c);
            }
            '"' if !escape_next => {
                in_string = !in_string;
                content.push(c);
            }
            '\\' if in_string => {
                escape_next = true;
                content.push(c);
            }
            _ => {
                escape_next = false;
                content.push(c);
            }
        }
    }
    
    Err("Unclosed attribute")
}

fn parse_attribute(input: &str) -> Result<(String, &str), &'static str> {
    // Check for outer attribute #[...]
    if input.starts_with("#[") {
        let content = &input[2..];
        let (attr_content, remaining) = parse_attribute_content(content)?;
        if remaining.starts_with(']') {
            return Ok((attr_content.trim().to_string(), &remaining[1..]));
        }
    }
    // Check for inner attribute #![...]
    else if input.starts_with("#![") {
        let content = &input[3..];
        let (attr_content, remaining) = parse_attribute_content(content)?;
        if remaining.starts_with(']') {
            let mut full_content = "!".to_string();
            full_content.push_str(&attr_content);
            return Ok((full_content.trim().to_string(), &remaining[1..]));
        }
    }
    
    Err("Not an attribute")
}

fn main() {
    println!("=== Testing Improved Attribute Parser ===\n");
    
    let test_cases = vec![
        ("#[ai_opt]", "ai_opt"),
        ("#[derive(Clone, Debug)]", "derive(Clone, Debug)"),
        ("#[cfg(target_os = \"linux\")]", "cfg(target_os = \"linux\")"),
        ("#![feature(special)]", "!feature(special)"),
        ("#[allow(unused_variables, dead_code)]", "allow(unused_variables, dead_code)"),
        ("#[ai_opt] fn foo() {}", "ai_opt"),
    ];
    
    for (i, (input, expected)) in test_cases.iter().enumerate() {
        println!("Test {}: Parsing '{}'", i + 1, input);
        match parse_attribute(input) {
            Ok((attr, remaining)) => {
                if attr == *expected {
                    println!("  ✅ Parsed correctly: '{}'", attr);
                } else {
                    println!("  ❌ Expected '{}', got '{}'", expected, attr);
                }
                println!("  Remaining: '{}'", remaining);
            }
            Err(e) => {
                println!("  ❌ Parse error: {}", e);
            }
        }
        println!();
    }
    
    // Test PrimeZeta specific case
    println!("=== PrimeZeta Compatibility Test ===");
    let primezeta_code = "// CacheSafe raw pointers + #[ai_opt] for perfect AVX-512 / vectorization";
    println!("Code: {}", primezeta_code);
    
    // Extract just the attribute part
    if let Some(start) = primezeta_code.find("#[ai_opt]") {
        let attr_part = &primezeta_code[start..];
        match parse_attribute(attr_part) {
            Ok((attr, _)) => {
                println!("✅ Successfully parsed #[ai_opt] attribute: '{}'", attr);
            }
            Err(e) => {
                println!("❌ Failed to parse #[ai_opt]: {}", e);
            }
        }
    } else {
        println!("⚠️  #[ai_opt] not found in the string (it's in a comment)");
    }
}