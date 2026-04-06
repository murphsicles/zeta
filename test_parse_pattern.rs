// Simple test for pattern parsing

extern crate zetac;
use zetac::frontend::parser::pattern::parse_pattern;

fn main() {
    // Test parsing a type-annotated pattern
    let input = "x: i64";
    let result = parse_pattern(input);
    println!("Parsing '{}': {:?}", input, result);
    
    // Test parsing an identity type annotation
    let input2 = "s: string[identity:read]";
    let result2 = parse_pattern(input2);
    println!("Parsing '{}': {:?}", input2, result2);
}