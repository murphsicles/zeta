extern crate zetac;

use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== Testing PrimeZeta simple parsing ===");
    
    // Test 1: Simple comptime function
    let code1 = r#"
comptime fn simple() -> i64 {
    42
}
    "#;
    
    println!("\nTest 1: Simple comptime function");
    test_parsing(code1);
    
    // Test 2: Function with var keyword
    let code2 = r#"
comptime fn with_var() -> i64 {
    var x: i64 = 42
    x
}
    "#;
    
    println!("\nTest 2: Function with var keyword");
    test_parsing(code2);
    
    // Test 3: Array repeat syntax
    let code3 = r#"
comptime fn with_array_repeat() -> [4]i64 {
    [0; 4]
}
    "#;
    
    println!("\nTest 3: Array repeat syntax");
    test_parsing(code3);
}

fn test_parsing(code: &str) {
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("  ✅ Parsed successfully");
            println!("  AST has {} nodes", ast.len());
            if !remaining.trim().is_empty() {
                println!("  Remaining input: '{}'", remaining);
            }
            
            // Check for comptime functions
            let comptime_funcs: Vec<_> = ast.iter()
                .filter(|node| {
                    match node {
                        zetac::frontend::ast::AstNode::FuncDef { comptime_, .. } => *comptime_,
                        _ => false,
                    }
                })
                .collect();
            
            println!("  Found {} comptime functions", comptime_funcs.len());
        }
        Err(e) => {
            println!("  ❌ Parse error: {:?}", e);
        }
    }
}