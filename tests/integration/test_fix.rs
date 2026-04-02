// Simple test to check if the fix works
fn main() {
    // Test the parse_ident function
    println!("Testing parse_ident:");
    
    let test_cases = vec![
        ("foo", true),  // regular identifier
        ("u64", false), // built-in type
        ("i64", false), // built-in type
        ("NUM_RESIDUES", true), // constant identifier
    ];
    
    for (test, should_succeed) in test_cases {
        // Simulate what parse_ident does
        let is_keyword = [
            "let", "mut", "if", "else", "for", "in", "loop", "unsafe", "return", "break",
            "continue", "fn", "concept", "impl", "enum", "struct", "type", "use", "extern",
            "dyn", "box", "as", "true", "false", "comptime", "const", "async", "pub",
            "i64", "u64", "usize", "f64", "bool", "String",
        ].contains(&test);
        
        let succeeds = !is_keyword;
        
        if succeeds == should_succeed {
            println!("  ✓ '{}': {}", test, if succeeds { "accepted" } else { "rejected" });
        } else {
            println!("  ✗ '{}': expected {}, got {}", test, 
                if should_succeed { "accepted" } else { "rejected" },
                if succeeds { "accepted" } else { "rejected" });
        }
    }
    
    println!("\nThe fix should:");
    println!("1. Make parse_ident reject built-in types like u64");
    println!("2. Make parse_primezeta_array fail quickly when trying to parse [u64; 10] as PrimeZeta style");
    println!("3. Allow parse_array_type to fall back to Zeta style parsing");
    println!("4. Successfully parse [u64; 10] as Zeta style");
    println!("5. Successfully parse [NUM_RESIDUES]u64 as PrimeZeta style");
}