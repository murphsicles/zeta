use std::process::Command;
use std::fs;
use std::path::Path;

fn main() {
    println!("=== Testing Full Range Loop Compilation ===\n");

    // Create test file
    let test_code = r#"fn main() -> i64 {
    let mut sum = 0;
    for i in 1..10 {
        sum = sum + i;
    }
    return sum;
}"#;

    let test_file = "test_range_loop.z";
    fs::write(test_file, test_code).expect("Failed to write test file");

    println!("Test code written to {}\n", test_file);
    println!("Code:\n{}", test_code);

    // Try to compile using zetac CLI
    println!("\nAttempting to compile with zetac...");
    
    let output = Command::new("cargo")
        .args(["run", "--bin", "zetac", "--", test_file])
        .current_dir(".")
        .output()
        .expect("Failed to execute zetac");

    println!("Exit code: {}", output.status);
    println!("Stdout:\n{}", String::from_utf8_lossy(&output.stdout));
    println!("Stderr:\n{}", String::from_utf8_lossy(&output.stderr));

    // Clean up
    if Path::new(test_file).exists() {
        fs::remove_file(test_file).expect("Failed to remove test file");
    }
}