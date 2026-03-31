use std::fs;

fn main() {
    let code = fs::read_to_string("bootstrap/minimal_compiler.z").unwrap();
    
    // Find where "impl Parser {" starts
    if let Some(pos) = code.find("impl Parser {") {
        println!("Found 'impl Parser {{' at position {}", pos);
        
        // Show some bytes before and after
        let start = pos.saturating_sub(20);
        let end = (pos + 50).min(code.len());
        
        println!("Context ({} bytes):", end - start);
        for (i, ch) in code[start..end].char_indices() {
            println!("  {:4} (byte {:4}): {:?} (U+{:04X})", 
                     i + start, 
                     code[start..].as_bytes()[i],
                     ch,
                     ch as u32);
        }
        
        // Check if there are any non-ASCII characters
        println!("\nChecking for non-ASCII characters in 'impl Parser {{':");
        let impl_start = &code[pos..];
        for (i, ch) in impl_start.char_indices().take(50) {
            if !ch.is_ascii() {
                println!("  Non-ASCII at position {} (byte {}): {:?} (U+{:04X})", 
                         i, 
                         impl_start.as_bytes()[i],
                         ch,
                         ch as u32);
            }
        }
    } else {
        println!("Could not find 'impl Parser {{'");
    }
}