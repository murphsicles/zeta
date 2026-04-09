use zetac::frontend::parser::parser::parse_type;

fn main() {
    let test = "string";
    println!("Testing: '{}'", test);
    match parse_type(test) {
        Ok((remaining, result)) => {
            println!("  Success: '{}', Remaining: '{}'", result, remaining);
        }
        Err(e) => {
            println!("  Error: {:?}", e);
        }
    }
}