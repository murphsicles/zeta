// Test for type system integration
use zetac::middle::resolver::resolver::Resolver;
use zetac::frontend::ast::AstNode;
use zetac::middle::types::Type;

fn main() {
    println!("Testing type system integration...");
    
    // Create a resolver
    let mut resolver = Resolver::new();
    
    // Test 1: Check that Type enum is properly imported
    println!("Test 1: Type enum variants");
    let i32_type = Type::I32;
    let i64_type = Type::I64;
    println!("  Type::I32: {:?}", i32_type);
    println!("  Type::I64: {:?}", i64_type);
    
    // Test 2: Check string_to_type conversion
    println!("\nTest 2: string_to_type conversion");
    let test_cases = vec!["i32", "i64", "bool", "str", "()"];
    for s in test_cases {
        let ty = resolver.string_to_type(s);
        println!("  '{}' -> {:?}", s, ty);
    }
    
    // Test 3: Check function signature storage
    println!("\nTest 3: Function signature with Type enum");
    // This would test that funcs HashMap stores Type enum properly
    // but we need to register a function first
    
    println!("\nAll tests completed!");
}