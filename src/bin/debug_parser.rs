use std::fs;
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    
    let code = if args.len() > 1 {
        fs::read_to_string(&args[1]).unwrap_or_else(|_| {
            eprintln!("Failed to read file: {}", args[1]);
            std::process::exit(1);
        })
    } else {
        r#"
        fn main() {
            let x = ::new();
        }
        "#
        .to_string()
    };

    println!("Parsing code:\n{}", code);
    println!("---");

    let result = parse_zeta(&code);
    println!("Result: {:?}", result);

    match result {
        Ok((remaining, ast)) => {
            println!("Remaining: '{}'", remaining);
            println!("AST: {:?}", ast);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}
