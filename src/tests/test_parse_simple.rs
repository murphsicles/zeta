use std::fs;

fn main() {
    // Read the test file
    let code = fs::read_to_string("simple_const_test.z").unwrap();
    
    println!("Testing parsing of: {}", code.trim());
    
    // We need to use the zetac crate, but let's just compile and run a simple test
    println!("This would test const parsing if we could compile against zetac");
}