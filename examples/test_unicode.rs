use zetac::frontend::parser::expr::parse_expr;

fn main() {
    println!("Testing Unicode identifier support in Zeta v0.3.8");
    
    let test_cases = vec![
        ("my_var", true),
        ("π", true),
        ("α", true),
        ("β", true),
        ("変数", true),
        ("标识符", true),
        ("π_approx", true),
        ("αβγ123", true),
        ("123var", false),
        ("let", false),
    ];
    
    for (input, should_work) in test_cases {
        println!("\nTesting: '{}'", input);
        match parse_expr(input) {
            Ok((remaining, ast)) => {
                if should_work {
                    println!("  ✓ Success!");
                    println!("  AST: {:?}", ast);
                } else {
                    println!("  ✗ Should have failed but succeeded");
                }
            }
            Err(e) => {
                if !should_work {
                    println!("  ✓ Failed as expected");
                } else {
                    println!("  ✗ Should have succeeded but failed: {:?}", e);
                }
            }
        }
    }
    
    println!("\n=== Unicode identifier test complete ===");
}