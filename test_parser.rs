use zeta::frontend::parser::parser::parse_generic_params_as_enum;

fn main() {
    let test_input = "(maybe_value: Option<i64>) -> i64 {\n            match maybe_value {\n              Some(value) => value,\n                None => 0,\n            }\n        }\n        ";
    
    println!("Testing parse_generic_params_as_enum with input:");
    println!("{:?}", test_input);
    
    match parse_generic_params_as_enum(test_input) {
        Ok((remaining, result)) => {
            println!("Success! Remaining: {:?}", remaining);
            println!("Result: {:?}", result);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}