use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let test_cases = vec![
        // Test 1: bool return type
        r#"fn test_bool() -> bool { return true }"#,
        // Test 2: array return type (Zeta style)
        r#"fn test_array() -> [u64; 10] { return [0; 10] }"#,
        // Test 3: array return type (PrimeZeta style)
        r#"fn test_primezeta_array() -> [10]u64 { return [0; 10] }"#,
        // Test 4: type alias
        r#"type MyInt = u64"#,
        // Test 5: comptime function
        r#"comptime fn generate() -> u64 { return 42 }"#,
    ];
    
    for (i, test_code) in test_cases.iter().enumerate() {
        println!("\nTest {}: {}", i + 1, test_code);
        match parse_zeta(test_code) {
            Ok((remaining, ast)) => {
                if remaining.trim().is_empty() {
                    println!("  ✅ Parsed successfully!");
                    println!("  AST: {:?}", ast);
                } else {
                    println!("  ⚠️  Partial parse. Remaining: '{}'", remaining);
                    println!("  AST: {:?}", ast);
                }
            }
            Err(e) => {
                println!("  ❌ Parse failed: {:?}", e);
            }
        }
    }
}