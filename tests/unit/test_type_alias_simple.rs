use std::fs;

fn main() {
    let content = fs::read_to_string("test_type_alias.z").unwrap();
    println!("Test file content:\n{}", content);
    
    // Simple test of type alias parsing
    let test_code = "type MyInt = i64;";
    println!("Testing: {}", test_code);
}