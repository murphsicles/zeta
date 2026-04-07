use zetac::frontend::parser::parser::skip_ws_and_comments;
use nom::bytes::complete::tag;

fn main() {
    let input = "Ok(x) = result";
    
    // First parse path
    use zetac::frontend::parser::parser::parse_path;
    match parse_path(input) {
        Ok((remaining, path)) => {
            println!("parse_path success: {:?}", path);
            println!("  remaining: '{}'", remaining);
            
            // Then skip whitespace
            match skip_ws_and_comments(remaining) {
                Ok((remaining2, _)) => {
                    println!("skip_ws_and_comments success");
                    println!("  remaining: '{}'", remaining2);
                    
                    // Then check for '('
                    match tag("(")(remaining2) {
                        Ok((remaining3, _)) => {
                            println!("tag('(') success");
                            println!("  remaining: '{}'", remaining3);
                        }
                        Err(e) => {
                            println!("tag('(') error: {:?}", e);
                        }
                    }
                }
                Err(e) => {
                    println!("skip_ws_and_comments error: {:?}", e);
                }
            }
        }
        Err(e) => {
            println!("parse_path error: {:?}", e);
        }
    }
}