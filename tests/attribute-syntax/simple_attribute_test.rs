fn main() {
    // Test the attribute parser directly
    use zetac::frontend::parser::parser::parse_attribute;
    use zetac::frontend::parser::parser::parse_attributes;
    
    println!("=== Testing Attribute Parser Directly ===\n");
    
    // Test 1: Simple attribute
    let test1 = "#[test] fn foo() {}";
    println!("Test 1: Parsing '#[test]'");
    match parse_attribute(test1) {
        Ok((remaining, attr)) => {
            println!("  ✅ Parsed attribute: '{}'", attr);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    
    // Test 2: Attribute with arguments
    let test2 = "#[derive(Clone, Debug)] struct Point {}";
    println!("\nTest 2: Parsing '#[derive(Clone, Debug)]'");
    match parse_attribute(test2) {
        Ok((remaining, attr)) => {
            println!("  ✅ Parsed attribute: '{}'", attr);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    
    // Test 3: Multiple attributes
    let test3 = "#[cfg(test)] #[allow(unused)] fn bar() {}";
    println!("\nTest 3: Parsing multiple attributes");
    match parse_attributes(test3) {
        Ok((remaining, attrs)) => {
            println!("  ✅ Parsed {} attributes:", attrs.len());
            for (i, attr) in attrs.iter().enumerate() {
                println!("    Attribute {}: '{}'", i, attr);
            }
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    
    // Test 4: #[ai_opt] attribute
    let test4 = "#[ai_opt] comptime fn optimized() {}";
    println!("\nTest 4: Parsing '#[ai_opt]'");
    match parse_attribute(test4) {
        Ok((remaining, attr)) => {
            println!("  ✅ Parsed attribute: '{}'", attr);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    
    // Test 5: Inner attribute #![attr]
    let test5 = "#![feature(special)] mod foo {}";
    println!("\nTest 5: Parsing inner attribute '#![feature(special)]'");
    // Note: We need to check if inner attributes are supported
    println!("  ⚠️  Inner attributes not yet implemented");
}