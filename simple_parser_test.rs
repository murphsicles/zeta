use std::fs;

fn main() {
    // Read the bootstrap file
    let code = match fs::read_to_string("bootstrap/minimal_compiler.z") {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Failed to read file: {}", e);
            return;
        }
    };
    
    // Find where "impl Parser {" starts
    if let Some(pos) = code.find("impl Parser {") {
        println!("Found 'impl Parser {{' at position {}", pos);
        
        // Extract just the impl block (first 1000 chars)
        let impl_block = &code[pos..pos + 1000.min(code.len() - pos)];
        println!("Impl block (first 1000 chars):");
        println!("{}", impl_block);
        
        // Check what comes before the impl block
        let before = &code[pos.saturating_sub(100)..pos];
        println!("\nWhat comes before 'impl Parser {{':");
        println!("{}", before);
    } else {
        println!("Could not find 'impl Parser {{'");
    }
}