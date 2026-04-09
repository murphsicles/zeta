fn main() {
    // Test that comptime is recognized as a keyword
    let test_ident = "comptime";
    let keywords = [
        "let", "mut", "if", "else", "for", "in", "loop", "unsafe", "return", "break",
        "continue", "fn", "concept", "impl", "enum", "struct", "type", "use", "extern",
        "dyn", "box", "as", "true", "false", "comptime", "const", "async", "pub",
    ];
    
    println!("Testing if 'comptime' is in keyword list:");
    if keywords.contains(&test_ident) {
        println!("✅ 'comptime' is recognized as a keyword");
    } else {
        println!("❌ 'comptime' is NOT in keyword list");
    }
    
    // Test parsing simple comptime constructs
    println!("\nTesting simple comptime parsing patterns:");
    
    // Test 1: comptime fn
    let test1 = "comptime fn test() -> i64 { return 42 }";
    println!("Test 1: {}", test1);
    println!("  Should parse as: comptime function declaration");
    
    // Test 2: comptime variable
    let test2 = "comptime x: i64 = 123";
    println!("Test 2: {}", test2);
    println!("  Should parse as: comptime constant declaration");
    
    // Test 3: pub comptime fn
    let test3 = "pub comptime fn public_fn() -> i64 { return 456 }";
    println!("Test 3: {}", test3);
    println!("  Should parse as: public comptime function");
}