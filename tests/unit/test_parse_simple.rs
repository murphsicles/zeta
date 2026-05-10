use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    // Test the simplest possible comptime function
    let code = "comptime fn test() -> i64 { return 1 }";
    
    println!("Testing: {}", code);
    match parse_zeta(code) {
        Ok((remaining, ast)) => {
            println!("Success!");
            println!("Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}