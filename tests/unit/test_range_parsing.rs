use zeta::frontend::parser::expr::parse_full_expr;

fn main() {
    // Test 1: Simple range expression
    let test1 = "1..10";
    match parse_full_expr(test1) {
        Ok((remaining, ast)) => {
            println!("Test 1 - Parsed: {:?}", ast);
            println!("Remaining: '{}'", remaining);
        }
        Err(e) => println!("Test 1 - Error: {:?}", e),
    }

    // Test 2: Range in for loop context
    let test2 = "for i in 1..10 { }";
    // We'll need to test this with the statement parser
    println!("\nTest 2 - For loop with range");
    
    // Test 3: Variable in range
    let test3 = "start..end";
    match parse_full_expr(test3) {
        Ok((remaining, ast)) => {
            println!("Test 3 - Parsed: {:?}", ast);
            println!("Remaining: '{}'", remaining);
        }
        Err(e) => println!("Test 3 - Error: {:?}", e),
    }
}