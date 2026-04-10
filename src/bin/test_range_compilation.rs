use zetac::compile_and_run_zeta;

fn main() {
    println!("=== Testing Range Loop Compilation ===\n");

    // Test 1: Simple range loop
    let test1 = r#"
fn main() -> i64 {
    let mut sum = 0;
    for i in 1..10 {
        sum = sum + i;
    }
    return sum;
}
"#;

    println!("Test 1: Sum of 1..9 (exclusive range)");
    match compile_and_run_zeta(test1) {
        Ok(result) => println!("  Result: {} (expected: 45)", result),
        Err(e) => println!("  Error: {}", e),
    }

    // Test 2: Inclusive range loop
    let test2 = r#"
fn main() -> i64 {
    let mut sum = 0;
    for i in 1..=10 {
        sum = sum + i;
    }
    return sum;
}
"#;

    println!("\nTest 2: Sum of 1..=10 (inclusive range)");
    match compile_and_run_zeta(test2) {
        Ok(result) => println!("  Result: {} (expected: 55)", result),
        Err(e) => println!("  Error: {}", e),
    }

    // Test 3: With constant
    let test3 = r#"
const N: i64 = 5;

fn main() -> i64 {
    let mut sum = 0;
    for i in 1..N {
        sum = sum + i;
    }
    return sum;
}
"#;

    println!("\nTest 3: Sum of 1..N where N=5");
    match compile_and_run_zeta(test3) {
        Ok(result) => println!("  Result: {} (expected: 10)", result),
        Err(e) => println!("  Error: {}", e),
    }

    // Test 4: PrimeZeta-style loop
    let test4 = r#"
const MODULUS: i64 = 100;

fn main() -> i64 {
    let mut sum = 0;
    for i in 1..MODULUS {
        sum = sum + i;
    }
    return sum;
}
"#;

    println!("\nTest 4: PrimeZeta-style loop (1..MODULUS where MODULUS=100)");
    match compile_and_run_zeta(test4) {
        Ok(result) => {
            let expected = (1..100).sum::<i64>();
            println!("  Result: {} (expected: {})", result, expected);
        }
        Err(e) => println!("  Error: {}", e),
    }
}