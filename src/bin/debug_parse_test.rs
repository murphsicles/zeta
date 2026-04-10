use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Enable debug output
    unsafe { std::env::set_var("RUST_LOG", "debug"); }
    
    let test_cases = vec![
        "fn test(a: i64) -> i64 { a }",
        "fn simple(x: i32) -> i32 { x }",
        "fn works() -> i64 { 42 }",
    ];
    
    for (i, code) in test_cases.iter().enumerate() {
        println!("\n=== Test case {}: '{}' ===", i, code);
        match parse_zeta(code) {
            Ok((remaining, ast)) => {
                println!("SUCCESS!");
                println!("Remaining: '{}'", remaining);
                println!("AST: {:?}", ast);
            }
            Err(e) => {
                println!("ERROR: {:?}", e);
            }
        }
    }
}