fn main() {
    // Simple test of while loop parsing
    let code = "while i < 10 { i = i + 1 }";
    println!("Testing: {}", code);
    
    // We'll manually test the parsing logic
    // First check if "while" is recognized
    if code.starts_with("while") {
        println!("✓ Starts with 'while'");
        
        // Check if we can parse "i < 10"
        let after_while = &code[5..]; // Skip "while"
        println!("After 'while': '{}'", after_while);
        
        // The condition should be "i < 10"
        // Look for {
        if let Some(brace_pos) = after_while.find('{') {
            let condition = &after_while[..brace_pos].trim();
            println!("Condition: '{}'", condition);
            
            // The body should be "{ i = i + 1 }"
            let body = &after_while[brace_pos..].trim();
            println!("Body: '{}'", body);
            
            if body.starts_with('{') && body.ends_with('}') {
                println!("✓ Body has braces");
            }
        }
    }
}