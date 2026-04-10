use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Enable debug output
    unsafe { std::env::set_var("RUST_LOG", "debug"); }
    
    let test_cases = vec![
        "fn test(a: i64) -> i64 { a }",
        "fn simple(x: i32) -> i32 { x }",
        "fn works() -> i64 { 42 }",
        "fn with_ref(x: &i64) -> i64 { *x }",
        "fn with_mut_ref(x: &mut i64) -> i64 { *x }",
        "fn with_string(s: String) -> String { s }",
        "fn with_array(arr: [i64; 10]) -> i64 { arr[0] }",
        "fn with_tuple(t: (i64, i32)) -> i64 { t.0 }",
        "fn multiple(a: i64, b: i64) -> i64 { a + b }",
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