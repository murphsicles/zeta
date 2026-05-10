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
        
        // Parse manually
        let mut chars = test.chars().collect::<Vec<_>>();
        let mut i = 0;
        
        // Skip '['
        if chars[i] == '[' {
            i += 1;
            println!("  Starts with '['");
            
            // Try to parse PrimeZeta style: [N]T
            let mut is_primezeta = false;
            let mut size_start = i;
            
            // Parse size (digits or identifier)
            while i < chars.len() && (chars[i].is_ascii_digit() || chars[i].is_ascii_alphabetic() || chars[i] == '_') {
                i += 1;
            }
            
            if i > size_start && i < chars.len() && chars[i] == ']' {
                // We have [N]
                let size = &test[size_start..i];
                i += 1; // Skip ']'
                
                // Check if there's a type after
                if i < chars.len() {
                    let elem_type = &test[i..];
                    println!("  PrimeZeta style: [{}]{}", size, elem_type);
                    is_primezeta = true;
                }
            }
            
            if !is_primezeta {
                println!("  Not PrimeZeta style, trying Zeta style");
                // Reset
                i = 1; // After '['
                
                // Parse type
                let mut type_start = i;
                while i < chars.len() && chars[i] != ';' && chars[i] != ']' {
                    i += 1;
                }
                
                if i < chars.len() {
                    let elem_type = &test[type_start..i];
                    
                    if chars[i] == ';' {
                        // Has size: [T; N]
                        i += 1; // Skip ';'
                        // Skip whitespace
                        while i < chars.len() && chars[i] == ' ' {
                            i += 1;
                        }
                        
                        let mut size_start = i;
                        while i < chars.len() && chars[i] != ']' {
                            i += 1;
                        }
                        
                        if i < chars.len() && chars[i] == ']' {
                            let size = &test[size_start..i];
                            println!("  Zeta style fixed: [{}; {}]", elem_type, size);
                        }
                    } else if chars[i] == ']' {
                        // No size: [T]
                        println!("  Zeta style unsized: [{}]", elem_type);
                    }
                }
            }
        }
    }
}