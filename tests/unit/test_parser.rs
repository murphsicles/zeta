use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let source = r#"
const MODULUS: u64 = 30030
const NUM_RESIDUES: usize = 5760
    "#;
    
    match parse_zeta(source) {
        Ok((remaining, ast)) => {
            println!("Parse successful!");
            println!("Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}