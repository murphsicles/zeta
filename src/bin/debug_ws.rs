use zetac::frontend::parser::parser::skip_ws_and_comments0;

fn main() {
    let inputs = ["{", " {", "{ ", "  {  ", "// comment\n{", "/* comment */{"];

    for input in inputs {
        println!("Testing: {:?}", input);
        match skip_ws_and_comments0(input) {
            Ok((remaining, _)) => println!("  OK: remaining = {:?}", remaining),
            Err(e) => println!("  Err: {:?}", e),
        }
    }
}
