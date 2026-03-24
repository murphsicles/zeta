use zetac::frontend::parser::stmt::parse_stmt;

fn main() {
    println!("Testing parse_stmt(\"a += b\")");
    match parse_stmt("a += b") {
        Ok((remaining, ast)) => {
            println!("Success! Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}