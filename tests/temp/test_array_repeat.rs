use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::Resolver;

fn main() {
    println!("=== Testing Array Repeat with Variable Size ===\n");
    
    // Test 1: Prime sieve function using [true; limit] syntax
    let prime_sieve_code = r#"
comptime fn prime_sieve(limit: i64) -> [limit]bool {
    let mut sieve = [true; limit];
    sieve[0] = false;
    sieve[1] = false;
    
    for i in 2..limit {
        if sieve[i] {
            let mut j = i * i;
            while j < limit {
                sieve[j] = false;
                j += i;
            }
        }
    }
    sieve
}
    "#;
    
    println!("Test 1: Prime sieve with [true; limit] syntax");
    match parse_zeta(prime_sieve_code) {
        Ok((remaining, ast)) => {
            println!("  ✅ Parsed successfully");
            println!("  AST has {} nodes", ast.len());
            
            // Now try type checking
            let mut resolver = Resolver::new();
            let typecheck_result = resolver.typecheck(&ast);
            println!("  Type check result: {}", typecheck_result);
            
            if !typecheck_result {
                println!("  ❌ Type checking failed");
            } else {
                println!("  ✅ Type checking passed");
            }
            
            if !remaining.trim().is_empty() {
                println!("  Remaining input: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
    
    // Test 2: Simple array repeat with variable
    let simple_code = r#"
fn test_array(limit: usize) -> [bool; limit] {
    [true; limit]
}
    "#;
    
    println!("\nTest 2: Simple array repeat with variable");
    match parse_zeta(simple_code) {
        Ok((remaining, ast)) => {
            println!("  ✅ Parsed successfully");
            println!("  AST has {} nodes", ast.len());
            
            let mut resolver = Resolver::new();
            let typecheck_result = resolver.typecheck(&ast);
            println!("  Type check result: {}", typecheck_result);
            
            if !typecheck_result {
                println!("  ❌ Type checking failed");
            } else {
                println!("  ✅ Type checking passed");
            }
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
}