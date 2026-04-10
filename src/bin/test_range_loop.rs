use zetac::frontend::parser::expr::parse_full_expr;
use zetac::frontend::parser::stmt::parse_stmt;

fn main() {
    println!("=== Testing Range Expression Parsing ===\n");

    // Test 1: Simple range expression
    println!("Test 1: '1..10'");
    match parse_full_expr("1..10") {
        Ok((remaining, ast)) => {
            println!("  AST: {:?}", ast);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => println!("  Error: {:?}", e),
    }

    println!("\nTest 2: 'start..end'");
    match parse_full_expr("start..end") {
        Ok((remaining, ast)) => {
            println!("  AST: {:?}", ast);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => println!("  Error: {:?}", e),
    }

    println!("\nTest 3: '1 .. 10' (with spaces)");
    match parse_full_expr("1 .. 10") {
        Ok((remaining, ast)) => {
            println!("  AST: {:?}", ast);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => println!("  Error: {:?}", e),
    }

    println!("\n=== Testing For Loop with Range ===\n");

    println!("Test 4: 'for i in 1..10 {{}}'");
    match parse_stmt("for i in 1..10 {}") {
        Ok((remaining, ast)) => {
            println!("  AST: {:?}", ast);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => println!("  Error: {:?}", e),
    }

    println!("\nTest 5: 'for i in 1..10 {{ println!(i); }}'");
    match parse_stmt("for i in 1..10 { println!(i); }") {
        Ok((remaining, ast)) => {
            println!("  AST: {:?}", ast);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => println!("  Error: {:?}", e),
    }

    println!("\nTest 6: Inclusive range '1..=10'");
    match parse_full_expr("1..=10") {
        Ok((remaining, ast)) => {
            println!("  AST: {:?}", ast);
            println!("  Remaining: '{}'", remaining);
        }
        Err(e) => println!("  Error: {:?}", e),
    }
}