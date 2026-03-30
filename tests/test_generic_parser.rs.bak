use src::frontend::parser::parser::*;
use src::frontend::parser::top_level::parse_struct;

fn main() {
    println!("Testing v0.5.0 Generic Parser Implementation");
    println!("============================================\n");
    
    // Test 1: Pointer types
    println!("1. Testing pointer types:");
    let tests = [
        ("*const i32", "*const i32"),
        ("*mut String", "*mut String"),
        ("*const [T]", "*const [T]"),
    ];
    
    for (input, expected) in tests.iter() {
        match parse_type(input) {
            Ok((rest, result)) => {
                if result == *expected && rest.is_empty() {
                    println!("   ✓ Parsed: '{}' -> '{}'", input, result);
                } else {
                    println!("   ✗ Parsed: '{}' -> '{}' (expected '{}', rest: '{}')", 
                            input, result, expected, rest);
                }
            }
            Err(e) => println!("   ✗ Failed to parse '{}': {:?}", input, e),
        }
    }
    
    // Test 2: References with lifetimes
    println!("\n2. Testing references with lifetimes:");
    let tests = [
        ("&'a T", "&'a T"),
        ("&'a mut T", "&'a mut T"),
        ("&'static str", "&'static str"),
    ];
    
    for (input, expected) in tests.iter() {
        match parse_type(input) {
            Ok((rest, result)) => {
                if result == *expected && rest.is_empty() {
                    println!("   ✓ Parsed: '{}' -> '{}'", input, result);
                } else {
                    println!("   ✗ Parsed: '{}' -> '{}' (expected '{}', rest: '{}')", 
                            input, result, expected, rest);
                }
            }
            Err(e) => println!("   ✗ Failed to parse '{}': {:?}", input, e),
        }
    }
    
    // Test 3: Trait bounds
    println!("\n3. Testing trait bounds:");
    let tests = [
        ("T: Clone", "T: Clone"),
        ("T: Clone + Display", "T: Clone + Display"),
        ("T: std::fmt::Debug", "T: std::fmt::Debug"),
    ];
    
    for (input, expected) in tests.iter() {
        match parse_generic_param(input) {
            Ok((rest, result)) => {
                if result == *expected && rest.is_empty() {
                    println!("   ✓ Parsed: '{}' -> '{}'", input, result);
                } else {
                    println!("   ✗ Parsed: '{}' -> '{}' (expected '{}', rest: '{}')", 
                            input, result, expected, rest);
                }
            }
            Err(e) => println!("   ✗ Failed to parse '{}': {:?}", input, e),
        }
    }
    
    // Test 4: Where clauses
    println!("\n4. Testing where clauses:");
    let test = "where T: Clone, U: Debug + Display";
    match parse_where_clause(test) {
        Ok((rest, result)) => {
            println!("   ✓ Parsed: '{}'", test);
            for (param, bounds) in result {
                println!("     {}: {}", param, bounds.join(" + "));
            }
            if !rest.is_empty() {
                println!("     (rest: '{}')", rest);
            }
        }
        Err(e) => println!("   ✗ Failed to parse '{}': {:?}", test, e),
    }
    
    // Test 5: Complex struct with generics
    println!("\n5. Testing complex struct with generics:");
    let test = "struct Result<T, E> where T: Clone, E: Debug { ok: T, err: E }";
    match parse_struct(test) {
        Ok((rest, result)) => {
            println!("   ✓ Parsed struct");
            if let crate::frontend::ast::AstNode::StructDef { name, generics, lifetimes, .. } = result {
                println!("     Name: {}", name);
                println!("     Generics: {:?}", generics);
                println!("     Lifetimes: {:?}", lifetimes);
            }
            if !rest.is_empty() {
                println!("     (rest: '{}')", rest);
            }
        }
        Err(e) => println!("   ✗ Failed to parse: {:?}", e),
    }
    
    // Test 6: Complex type paths
    println!("\n6. Testing complex type paths:");
    let tests = [
        ("std::collections::HashMap<K, V>", "std::collections::HashMap<K, V>"),
        ("Vec<Result<T, E>>", "Vec<Result<T, E>>"),
    ];
    
    for (input, expected) in tests.iter() {
        match parse_type(input) {
            Ok((rest, result)) => {
                if result == *expected && rest.is_empty() {
                    println!("   ✓ Parsed: '{}' -> '{}'", input, result);
                } else {
                    println!("   ✗ Parsed: '{}' -> '{}' (expected '{}', rest: '{}')", 
                            input, result, expected, rest);
                }
            }
            Err(e) => println!("   ✗ Failed to parse '{}': {:?}", input, e),
        }
    }
    
    println!("\n============================================");
    println!("Parser implementation complete!");
}