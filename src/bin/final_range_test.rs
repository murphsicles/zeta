use std::process::Command;
use std::fs;

fn test_range(code: &str, expected: i64, name: &str) -> bool {
    let file = format!("test_{}.z", name);
    fs::write(&file, code).expect("Failed to write test file");

    let output = Command::new("cargo")
        .args(["run", "--bin", "zetac", "--", &file])
        .current_dir(".")
        .output()
        .expect("Failed to execute zetac");

    let _ = fs::remove_file(&file);

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        if let Some(line) = stdout.lines().find(|l| l.contains("Result:")) {
            if let Some(num_str) = line.split("Result:").nth(1) {
                if let Ok(num) = num_str.trim().parse::<i64>() {
                    return num == expected;
                }
            }
        }
    }
    false
}

fn main() {
    println!("=== FINAL RANGE LOOP TEST SUITE ===\n");
    println!("Testing v0.3.25 Range Loop Implementation\n");

    let mut passed = 0;
    let mut total = 0;

    // Test 1: Basic exclusive range
    total += 1;
    let test1 = r#"fn main() -> i64 {
    let mut sum = 0;
    for i in 1..10 {
        sum = sum + i;
    }
    return sum;
}"#;
    if test_range(test1, 45, "basic_exclusive") {
        println!("✅ Test 1: Basic exclusive range 1..10 = 45");
        passed += 1;
    } else {
        println!("❌ Test 1: Basic exclusive range FAILED");
    }

    // Test 2: Inclusive range
    total += 1;
    let test2 = r#"fn main() -> i64 {
    let mut sum = 0;
    for i in 1..=10 {
        sum = sum + i;
    }
    return sum;
}"#;
    if test_range(test2, 55, "inclusive_range") {
        println!("✅ Test 2: Inclusive range 1..=10 = 55");
        passed += 1;
    } else {
        println!("❌ Test 2: Inclusive range FAILED");
    }

    // Test 3: Variable bounds
    total += 1;
    let test3 = r#"fn main() -> i64 {
    let start = 5;
    let end = 10;
    let mut sum = 0;
    for i in start..end {
        sum = sum + i;
    }
    return sum;
}"#;
    if test_range(test3, 35, "variable_bounds") {
        println!("✅ Test 3: Variable bounds start=5..end=10 = 35");
        passed += 1;
    } else {
        println!("❌ Test 3: Variable bounds FAILED");
    }

    // Test 4: Empty range
    total += 1;
    let test4 = r#"fn main() -> i64 {
    let mut sum = 0;
    for i in 10..5 {
        sum = sum + i;
    }
    return sum;
}"#;
    if test_range(test4, 0, "empty_range") {
        println!("✅ Test 4: Empty range 10..5 = 0");
        passed += 1;
    } else {
        println!("❌ Test 4: Empty range FAILED");
    }

    // Test 5: Complex loop body
    total += 1;
    let test5 = r#"fn main() -> i64 {
    let mut product = 1;
    for i in 1..6 {
        product = product * i;
    }
    return product;
}"#;
    if test_range(test5, 120, "factorial_range") { // 5! = 120
        println!("✅ Test 5: Factorial 1..6 = 120");
        passed += 1;
    } else {
        println!("❌ Test 5: Factorial range FAILED");
    }

    // Test 6: Nested loops
    total += 1;
    let test6 = r#"fn main() -> i64 {
    let mut sum = 0;
    for i in 1..4 {
        for j in 1..4 {
            sum = sum + i * j;
        }
    }
    return sum;
}"#;
    if test_range(test6, 36, "nested_loops") { // Sum of i*j for i,j in 1..3
        println!("✅ Test 6: Nested loops = 36");
        passed += 1;
    } else {
        println!("❌ Test 6: Nested loops FAILED");
    }

    println!("\n=== SUMMARY ===");
    println!("Passed: {}/{}", passed, total);
    
    if passed == total {
        println!("\n🎉 ALL TESTS PASSED! Range loops are READY for v0.3.25!");
        println!("\nPrimeZeta compatibility:");
        println!("- ✅ for i in 1..N works (with variable N)");
        println!("- ⚠️  for i in 1..MODULUS (needs constant system fix)");
        println!("- ✅ Range syntax parsing works");
        println!("- ✅ Loop execution works");
        println!("- ✅ Type inference works (i64 for numeric ranges)");
    } else {
        println!("\n⚠️  Some tests failed. Needs investigation.");
    }
}