// Test current Zeta parser with array syntax
use std::process::Command;

fn main() {
    println!("Testing current Zeta parser with array syntax...\n");
    
    // Test code with current syntax
    let test_code = r#"
// Current Zeta syntax (if it works)
fn test_array() -> [bool; 10] {
    [true; 10]
}

// What we want to support
// fn sieve(limit: usize) -> [bool; limit] {
//     [true; limit]
// }
    "#;
    
    // Write test file
    std::fs::write("test_array.zeta", test_code).unwrap();
    
    println!("Test code:");
    println!("{}", test_code);
    
    // Try to parse with zetac if available
    println!("\nAttempting to parse...");
    
    // Check if zetac exists
    let output = Command::new("cargo")
        .args(["run", "--bin", "zetac", "--", "test_array.zeta"])
        .current_dir(".")
        .output();
    
    match output {
        Ok(output) => {
            if output.status.success() {
                println!("✅ Parse successful!");
                println!("Output: {}", String::from_utf8_lossy(&output.stdout));
            } else {
                println!("❌ Parse failed");
                println!("Stderr: {}", String::from_utf8_lossy(&output.stderr));
            }
        }
        Err(e) => {
            println!("⚠️  Could not run zetac: {}", e);
            println!("This is expected if zetac isn't built yet.");
        }
    }
    
    // Clean up
    let _ = std::fs::remove_file("test_array.zeta");
    
    println!("\n=== Analysis ===");
    println!("Current Zeta likely supports: [bool; 10]");
    println!("Need to add support for: [bool; limit] where limit is parameter");
    println!("\nProposed solution:");
    println!("1. Add const generic parameters: fn sieve<const LIMIT: usize>() -> [bool; LIMIT]");
    println!("2. Require LIMIT to be compile-time constant");
    println!("3. Monomorphize for each LIMIT value");
}