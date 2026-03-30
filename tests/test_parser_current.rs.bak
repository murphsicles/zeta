use src::frontend::parser::parser::*;

fn main() {
    // Test current parser capabilities
    
    println!("Testing current parser capabilities:");
    println!("====================================\n");
    
    // Test 1: Generic structs
    println!("1. Testing generic structs:");
    let test1 = "struct Vec<T> { data: T }";
    match parse_type(test1) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test1, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test1, e),
    }
    
    // Test 2: Generic function calls
    println!("\n2. Testing generic function calls:");
    let test2 = "vec_new::<i32>()";
    match parse_type(test2) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test2, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test2, e),
    }
    
    // Test 3: Array types
    println!("\n3. Testing array types:");
    let test3a = "[T]";
    match parse_type(test3a) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test3a, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test3a, e),
    }
    
    let test3b = "[T; N]";
    match parse_type(test3b) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test3b, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test3b, e),
    }
    
    // Test 4: Trait bounds (should fail currently)
    println!("\n4. Testing trait bounds (expected to fail):");
    let test4 = "T: Clone + Display";
    match parse_generic_param(test4) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test4, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}' (expected): {:?}", test4, e),
    }
    
    // Test 5: Lifetime parameters
    println!("\n5. Testing lifetime parameters:");
    let test5 = "'a";
    match parse_lifetime_param(test5) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test5, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test5, e),
    }
    
    // Test 6: Reference types
    println!("\n6. Testing reference types:");
    let test6a = "&T";
    match parse_type(test6a) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test6a, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test6a, e),
    }
    
    let test6b = "&mut T";
    match parse_type(test6b) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test6b, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test6b, e),
    }
    
    // Test 7: Complex type paths
    println!("\n7. Testing complex type paths:");
    let test7 = "std::collections::HashMap<K, V>";
    match parse_type(test7) {
        Ok((rest, result)) => println!("   ✓ Parsed: '{}' -> '{}' (remaining: '{}')", test7, result, rest),
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test7, e),
    }
}