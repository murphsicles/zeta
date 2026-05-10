use zetac::frontend::parser::expr::parse_expr;

fn main() {
    // Test current behavior with static method calls
    let test_cases = vec![
        "Point::new(10, 20)",
        "Vec::<i32>::new()",
        "SomeType::static_method()",
        "foo.bar()", // instance method for comparison
    ];

    for test in test_cases {
        println!("Testing: {}", test);
        match parse_expr(test) {
            Ok((remaining, ast)) => {
                println!("  Success: {:?}", ast);
                println!("  Remaining: '{}'", remaining);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
            }
        }
        println!();
    }
}
