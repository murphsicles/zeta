use zetac::frontend::parser::expr::parse_full_expr;

fn main() {
    let tests = ["a < 5 && b > 0", "a < 5", "1 < 2 && 3 > 0", "true && false"];

    for code in tests {
        println!("Testing: {:?}", code);
        match parse_full_expr(code) {
            Ok((remaining, _ast)) => {
                println!("  Success! Remaining: {:?}", remaining);
            }
            Err(e) => {
                println!("  Error: {:?}", e);
            }
        }
    }
}
