use std::fs;

fn main() {
    let code = "impl Test { fn simple() -> i64 { 42 } }";
    println!("Testing code: '{}'", code);
    
    // Import the parser directly
    use zetac::frontend::parser::top_level::parse_zeta;
    
    match parse_zeta(code) {
        Ok((remaining, asts)) => {
            println!("Parse succeeded!");
            println!("Remaining: '{}'", remaining);
            println!("ASTs count: {}", asts.len());
        }
        Err(e) => {
            println!("Parse failed: {:?}", e);
        }
    }
}
