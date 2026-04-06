// Test parse_type_string with identity types

extern crate zetac;

use zetac::middle::resolver::new_resolver::NewResolver;

fn main() {
    let resolver = NewResolver::new();
    
    // Test parsing a simple type
    let result = resolver.parse_type_string("i64");
    println!("parse_type_string(\"i64\"): {:?}", result);
    
    // Test parsing an identity type
    let result2 = resolver.parse_type_string("string[identity:read]");
    println!("parse_type_string(\"string[identity:read]\"): {:?}", result2);
}