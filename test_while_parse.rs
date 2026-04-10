use zetac::frontend::parser::stmt::parse_stmt;

fn main() {
    let code = "while i < 10 { i = i + 1 }";
    println!("Testing while loop parsing: '{}'", code);
    
    match parse_stmt(code) {
        Ok((remaining, ast)) => {
            println!("Success! Parsed AST: {:?}", ast);
            if !remaining.is_empty() {
                println!("Remaining input: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}