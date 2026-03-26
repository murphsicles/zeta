use zetac::compile_and_run_zeta;

fn main() {
    println!("=== Testing Zeta compilation with constants ===");
    
    let code = r#"
const MAX_SIZE: i64 = 1024;

fn main() -> i64 {
    MAX_SIZE
}
"#;

    match compile_and_run_zeta(code) {
        Ok(result) => {
            println!("✓ Compilation and execution successful!");
            println!("  Result: {}", result);
            
            // The function returns 1024 (MAX_SIZE)
            if result == 1024 {
                println!("✓ Correct result: got 1024 as expected");
            } else {
                println!("✗ Unexpected result: got {}, expected 1024", result);
            }
        }
        Err(e) => {
            println!("✗ Compilation failed: {}", e);
        }
    }
    
    println!("\n=== Testing constant in expression ===");
    
    let code2 = r#"
const BASE: i64 = 100;
const OFFSET: i64 = 24;

fn main() -> i64 {
    BASE + OFFSET
}
"#;

    match compile_and_run_zeta(code2) {
        Ok(result) => {
            println!("✓ Compilation and execution successful!");
            println!("  Result: {}", result);
            
            // The function returns 124 (100 + 24)
            if result == 124 {
                println!("✓ Correct result: got 124 as expected");
            } else {
                println!("✗ Unexpected result: got {}, expected 124", result);
            }
        }
        Err(e) => {
            println!("✗ Compilation failed: {}", e);
        }
    }
}