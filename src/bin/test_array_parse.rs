extern crate zetac;

use zetac::frontend::parser::parser::parse_type;

fn main() {
    println!("Testing parse_type with [limit]bool");
    
    let input = "[limit]bool";
    match parse_type(input) {
        Ok((remaining, ty)) => {
            println!("✅ Success: parsed type = '{}'", ty);
            println!("   Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("❌ Error: {:?}", e);
        }
    }
    
    println!("\nTesting parse_type with [bool; limit]");
    
    let input2 = "[bool; limit]";
    match parse_type(input2) {
        Ok((remaining, ty)) => {
            println!("✅ Success: parsed type = '{}'", ty);
            println!("   Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("❌ Error: {:?}", e);
        }
    }
    
    println!("\nTesting parse_type with [bool; 10]");
    
    let input3 = "[bool; 10]";
    match parse_type(input3) {
        Ok((remaining, ty)) => {
            println!("✅ Success: parsed type = '{}'", ty);
            println!("   Remaining: '{}'", remaining);
        }
        Err(e) => {
            println!("❌ Error: {:?}", e);
        }
    }
}