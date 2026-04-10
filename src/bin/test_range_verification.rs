use std::process::Command;
use std::fs;

fn run_zeta_code(code: &str, test_name: &str) -> Result<i64, String> {
    let test_file = format!("test_{}.z", test_name);
    fs::write(&test_file, code).expect("Failed to write test file");

    let output = Command::new("cargo")
        .args(["run", "--bin", "zetac", "--", &test_file])
        .current_dir(".")
        .output()
        .expect("Failed to execute zetac");

    // Clean up
    let _ = fs::remove_file(&test_file);

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Extract result from output like "Result: 45"
        if let Some(line) = stdout.lines().find(|l| l.contains("Result:")) {
            if let Some(num_str) = line.split("Result:").nth(1) {
                if let Ok(num) = num_str.trim().parse::<i64>() {
                    return Ok(num);
                }
            }
        }
        Err("Could not parse result from output".to_string())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!("Compilation failed: {}", stderr))
    }
}

fn main() {
    println!("=== VERIFYING RANGE LOOP IMPLEMENTATION ===\n");

    // Test 1: Simple exclusive range
    let test1 = r#"fn main() -> i64 {
    let mut sum = 0;
    for i in 1..10 {
        sum = sum + i;
    }
    return sum;
}"#;

    println!("Test 1: Sum of 1..9 (exclusive range)");
    match run_zeta_code(test1, "exclusive_range") {
        Ok(result) => {
            let expected = (1..10).sum::<i64>();
            println!("  Result: {} (expected: {})", result, expected);
            if result == expected {
                println!("  ✅ PASS");
            } else {
                println!("  ❌ FAIL");
            }
        }
        Err(e) => println!("  Error: {}", e),
    }

    // Test 2: Inclusive range
    let test2 = r#"fn main() -> i64 {
    let mut sum = 0;
    for i in 1..=10 {
        sum = sum + i;
    }
    return sum;
}"#;

    println!("\nTest 2: Sum of 1..=10 (inclusive range)");
    match run_zeta_code(test2, "inclusive_range") {
        Ok(result) => {
            let expected = (1..=10).sum::<i64>();
            println!("  Result: {} (expected: {})", result, expected);
            if result == expected {
                println!("  ✅ PASS");
            } else {
                println!("  ❌ FAIL");
            }
        }
        Err(e) => println!("  Error: {}", e),
    }

    // Test 3: PrimeZeta-style with constant
    let test3 = r#"const MODULUS: i64 = 5;

fn main() -> i64 {
    let mut sum = 0;
    for i in 1..MODULUS {
        sum = sum + i;
    }
    return sum;
}"#;

    println!("\nTest 3: PrimeZeta-style with constant MODULUS=5");
    match run_zeta_code(test3, "primezeta_constant") {
        Ok(result) => {
            let expected = (1..5).sum::<i64>();
            println!("  Result: {} (expected: {})", result, expected);
            if result == expected {
                println!("  ✅ PASS");
            } else {
                println!("  ❌ FAIL");
            }
        }
        Err(e) => println!("  Error: {}", e),
    }

    // Test 4: Variable bounds
    let test4 = r#"fn main() -> i64 {
    let start = 5;
    let end = 10;
    let mut sum = 0;
    
    for i in start..end {
        sum = sum + i;
    }
    return sum;
}"#;

    println!("\nTest 4: Variable bounds start=5, end=10");
    match run_zeta_code(test4, "variable_bounds") {
        Ok(result) => {
            let expected = (5..10).sum::<i64>();
            println!("  Result: {} (expected: {})", result, expected);
            if result == expected {
                println!("  ✅ PASS");
            } else {
                println!("  ❌ FAIL");
            }
        }
        Err(e) => println!("  Error: {}", e),
    }

    // Test 5: Empty range
    let test5 = r#"fn main() -> i64 {
    let mut sum = 0;
    for i in 10..5 {  // Empty range
        sum = sum + i;
    }
    return sum;
}"#;

    println!("\nTest 5: Empty range 10..5");
    match run_zeta_code(test5, "empty_range") {
        Ok(result) => {
            println!("  Result: {} (expected: 0)", result);
            if result == 0 {
                println!("  ✅ PASS");
            } else {
                println!("  ❌ FAIL");
            }
        }
        Err(e) => println!("  Error: {}", e),
    }
}