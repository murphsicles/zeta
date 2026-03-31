use zetac::frontend::parser::top_level::parse_impl;

fn main() {
    let test_cases = vec![
        "impl Parser { fn new() -> Parser { Parser { input: \"\", pos: 0 } } }",
        "impl Parser {\n    fn new() -> Parser {\n        Parser { input: \"\", pos: 0 }\n    }\n}",
    ];
    
    for (i, code) in test_cases.iter().enumerate() {
        println!("Test {}: {}", i + 1, code);
        match parse_impl(code) {
            Ok((remaining, ast)) => {
                println!("  Success! Remaining: '{}'", remaining);
                println!("  AST: {:?}", ast);
            }
            Err(e) => {
                println!("  Parse error: {:?}", e);
            }
        }
        println!();
    }
}