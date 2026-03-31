use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let test_cases = vec![
        "fn main() -> i64 { 42 }",
        "fn add(a: i64, b: i64) -> i64 { a + b }",
        "struct Container<T> { value: T }",
        "fn identity<T>(x: T) -> T { x }",
        "let x = 42;",
        "if true { 1 } else { 0 }",
    ];
    
    for (i, code) in test_cases.iter().enumerate() {
        println!("Test {}: {}", i + 1, code);
        match parse_zeta(code) {
            Ok((remaining, asts)) => {
                println!("  Success! ASTs: {}, Remaining: '{}'", asts.len(), remaining);
                if !remaining.trim().is_empty() {
                    println!("  WARNING: Incomplete parse!");
                }
            }
            Err(e) => {
                println!("  Parse error: {:?}", e);
            }
        }
        println!();
    }
}