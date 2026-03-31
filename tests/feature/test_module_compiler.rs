use std::fs;

fn main() {
    let code = fs::read_to_string("test_simple_module.z").unwrap();
    
    // Try to compile and run
    match zetac::compile_and_run_zeta(&code) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error: {}", e),
    }
}