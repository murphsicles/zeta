use std::fs;

fn main() {
    // Read test file
    let code = fs::read_to_string("test_macro.zeta").expect("Failed to read test file");

    println!("Testing macro system for Zeta v0.5.0...");
    println!("Test code:\n{}", code);

    // Try to compile it
    match zetac::compile_and_run_zeta(&code) {
        Ok(result) => {
            println!("Compilation successful! Result: {}", result);
        }
        Err(e) => {
            println!("Compilation failed: {}", e);
            println!("This is expected for now as macro system is partially implemented.");
        }
    }
}
