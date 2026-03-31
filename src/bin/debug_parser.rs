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

    println!("Parsing code (first 500 chars):\n{}", &code[0..code.len().min(500)]);
    println!("---");

    let result = parse_zeta(&code);
    println!("Result: {:?}", result);

    match result {
        Ok((remaining, ast)) => {
            println!("Remaining length: {}", remaining.len());
            if !remaining.is_empty() {
                println!("First 200 chars of remaining: '{}'", &remaining[0..remaining.len().min(200)]);
                
                // Try to parse the remaining part
                println!("\nTrying to parse remaining part:");
                match parse_zeta(&remaining) {
                    Ok((remaining2, ast2)) => {
                        println!("  Parsed {} AST nodes from remaining", ast2.len());
                        println!("  Still remaining: {} chars", remaining2.len());
                        if !remaining2.is_empty() {
                            println!("  First 100 chars of still remaining: '{}'", &remaining2[0..remaining2.len().min(100)]);
                        }
                    }
                    Err(e) => {
                        println!("  Failed to parse remaining: {:?}", e);
                    }
                }
            }
            println!("AST count: {}", ast.len());
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}
