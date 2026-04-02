fn main() {
    println!("Testing array type parsing...");
    
    // Test cases
    let test_cases = [
        ("[u64; 10]", "Zeta style fixed array"),
        ("[u64]", "Zeta style unsized array"),
        ("[10]u64", "PrimeZeta style array"),
        ("[NUM_RESIDUES]u64", "PrimeZeta style with constant"),
    ];
    
    for (test, description) in test_cases.iter() {
        println!("\n{}: '{}'", description, test);
        
        // Manually trace through what should happen
        if test.starts_with('[') {
            println!("  Starts with '[' - good");
            
            if let Some(pos) = test.find(']') {
                println!("  Found ']' at position {}", pos);
                
                let after_bracket = &test[pos+1..];
                println!("  After ']': '{}'", after_bracket);
                
                if after_bracket.is_empty() {
                    println!("  ❌ No type after ']' - this is Zeta style unsized array");
                } else {
                    println!("  ✅ Has type after ']' - could be PrimeZeta style");
                }
            }
        }
    }
    
    println!("\n=== Parser logic ===");
    println!("parse_array_type tries PrimeZeta style first: [N]T");
    println!("If that fails, tries Zeta style: [T] or [T; N]");
    println!("PrimeZeta style requires: '[' + size + ']' + type");
    println!("Zeta style requires: '[' + type + (optional ';' + size) + ']'");
}