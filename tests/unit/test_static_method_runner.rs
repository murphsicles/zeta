// Test runner for static method test suite
// Runs all static method tests and reports results

use std::time::Instant;

fn main() {
    println!("=== Static Method Test Suite Runner ===");
    println!(
        "Starting at: {}",
        chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
    );
    println!();

    let start_time = Instant::now();

    // Run test categories
    println!("1. Running parser tests...");
    run_parser_tests();

    println!("\n2. Running integration tests...");
    run_integration_tests();

    println!("\n3. Running edge case tests...");
    run_edge_case_tests();

    println!("\n4. Running regression tests...");
    run_regression_tests();

    println!("\n5. Running error case tests...");
    run_error_case_tests();

    println!("\n6. Running known issue tests...");
    run_known_issue_tests();

    let duration = start_time.elapsed();
    println!("\n=== Test Suite Complete ===");
    println!("Total time: {:?}", duration);
    println!(
        "Finished at: {}",
        chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
    );
}

fn run_parser_tests() {
    let test_cases = vec![
        (
            "Simple static method",
            "fn main() { let p = Point::new(10, 20); }",
        ),
        (
            "Generic static method",
            "fn main() { let v = Vec::<i32>::new(); }",
        ),
        (
            "Nested path",
            "fn main() { let x = std::collections::HashMap::new(); }",
        ),
        (
            "With type args",
            "fn main() { let opt = Option::<bool>::None; }",
        ),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (name, code) in test_cases {
        match zetac::frontend::parser::top_level::parse_zeta(code) {
            Ok((remaining, _)) => {
                if remaining.is_empty() {
                    println!("  ✓ {}: PASS", name);
                    passed += 1;
                } else {
                    println!("  ✗ {}: FAIL - Unparsed: '{}'", name, remaining);
                    failed += 1;
                }
            }
            Err(e) => {
                println!("  ✗ {}: FAIL - Parse error: {:?}", name, e);
                failed += 1;
            }
        }
    }

    println!("  Parser tests: {}/{} passed", passed, passed + failed);
}

fn run_integration_tests() {
    let test_cases = vec![
        (
            "Basic static method",
            r#"
            struct Point { x: i64, y: i64 }
            fn point_new(x: i64, y: i64) -> Point { Point { x, y } }
            fn main() -> i64 { let p = point_new(10, 20); p.x + p.y }
        "#,
        ),
        (
            "Generic static method",
            r#"
            struct Vec<T> { data: [T] }
            fn vec_new<T>() -> Vec<T> { Vec { data: [] } }
            fn main() -> i64 { let v = vec_new::<i32>(); 0 }
        "#,
        ),
        (
            "Static method in impl block",
            r#"
            struct Point { x: i64, y: i64 }
            impl Point {
                fn new(x: i64, y: i64) -> Point { Point { x, y } }
                fn default() -> Point { Point { x: 0, y: 0 } }
            }
            fn main() -> i64 { let p1 = Point::new(10, 20); let p2 = Point::default(); p1.x + p2.x }
        "#,
        ),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (name, code) in test_cases {
        match zetac::frontend::parser::top_level::parse_zeta(code) {
            Ok((remaining, ast)) => {
                if !remaining.is_empty() {
                    println!("  ✗ {}: FAIL - Unparsed input", name);
                    failed += 1;
                    continue;
                }

                match zetac::middle::resolver::new_resolver::type_check(&ast) {
                    Ok(_) => {
                        println!("  ✓ {}: PASS (parse + type check)", name);
                        passed += 1;
                    }
                    Err(e) => {
                        println!("  ✗ {}: FAIL - Type check error: {:?}", name, e);
                        failed += 1;
                    }
                }
            }
            Err(e) => {
                println!("  ✗ {}: FAIL - Parse error: {:?}", name, e);
                failed += 1;
            }
        }
    }

    println!("  Integration tests: {}/{} passed", passed, passed + failed);
}

fn run_edge_case_tests() {
    let test_cases = vec![
        (
            "Chained static methods",
            "fn main() { let x = Vec::new().push(1).push(2); }",
        ),
        (
            "Mixed instance and static",
            r#"
            struct Point { x: i64, y: i64 }
            impl Point {
                fn new(x: i64, y: i64) -> Point { Point { x, y } }
                fn add(self, other: Point) -> Point { Point { x: self.x + other.x, y: self.y + other.y } }
            }
            fn main() { let p = Point::new(1, 2).add(Point::new(3, 4)); }
        "#,
        ),
        (
            "Complex generic",
            "fn main() { let x = HashMap::<String, Vec<i32>>::new(); }",
        ),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (name, code) in test_cases {
        match zetac::frontend::parser::top_level::parse_zeta(code) {
            Ok((remaining, _)) => {
                if remaining.is_empty() {
                    println!("  ✓ {}: PASS", name);
                    passed += 1;
                } else {
                    println!("  ✗ {}: FAIL - Unparsed: '{}'", name, remaining);
                    failed += 1;
                }
            }
            Err(e) => {
                println!("  ✗ {}: FAIL - Parse error: {:?}", name, e);
                failed += 1;
            }
        }
    }

    println!("  Edge case tests: {}/{} passed", passed, passed + failed);
}

fn run_regression_tests() {
    let test_cases = vec![
        (
            "Instance methods",
            r#"
            struct Point { x: i64, y: i64 }
            fn point_new(x: i64, y: i64) -> Point { Point { x, y } }
            fn point_add(self: Point, other: Point) -> Point { Point { x: self.x + other.x, y: self.y + other.y } }
            fn main() -> i64 { let p1 = point_new(1, 2); let p2 = point_new(3, 4); let p3 = p1.add(p2); p3.x + p3.y }
        "#,
        ),
        (
            "Module function calls",
            r#"
            fn add(a: i64, b: i64) -> i64 { a + b }
            fn main() -> i64 { add(1, 2) }
        "#,
        ),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (name, code) in test_cases {
        match zetac::frontend::parser::top_level::parse_zeta(code) {
            Ok((remaining, ast)) => {
                if !remaining.is_empty() {
                    println!("  ✗ {}: FAIL - Unparsed input", name);
                    failed += 1;
                    continue;
                }

                match zetac::middle::resolver::new_resolver::type_check(&ast) {
                    Ok(_) => {
                        println!("  ✓ {}: PASS", name);
                        passed += 1;
                    }
                    Err(e) => {
                        println!("  ✗ {}: FAIL - Type check error: {:?}", name, e);
                        failed += 1;
                    }
                }
            }
            Err(e) => {
                println!("  ✗ {}: FAIL - Parse error: {:?}", name, e);
                failed += 1;
            }
        }
    }

    println!("  Regression tests: {}/{} passed", passed, passed + failed);
}

fn run_error_case_tests() {
    let test_cases = vec![
        (
            "Missing method name",
            "fn main() { let x = Point::; }",
            "Expected method name",
        ),
        (
            "Missing path before ::",
            "fn main() { let x = ::new(); }",
            "Expected path before ::",
        ),
        (
            "Unclosed parentheses",
            "fn main() { let x = Point::new(10, 20; }",
            "Unclosed",
        ),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (name, code, expected_error) in test_cases {
        match zetac::frontend::parser::top_level::parse_zeta(code) {
            Ok((remaining, _)) => {
                if remaining.is_empty() {
                    println!(
                        "  ✗ {}: FAIL - Expected error but parsed successfully",
                        name
                    );
                    failed += 1;
                } else {
                    println!(
                        "  ✗ {}: FAIL - Partial parse, remaining: '{}'",
                        name, remaining
                    );
                    failed += 1;
                }
            }
            Err(e) => {
                let error_str = format!("{:?}", e);
                if error_str.contains(expected_error) {
                    println!("  ✓ {}: PASS - Got expected error", name);
                    passed += 1;
                } else {
                    println!("  ✗ {}: FAIL - Wrong error: {:?}", name, e);
                    failed += 1;
                }
            }
        }
    }

    println!("  Error case tests: {}/{} passed", passed, passed + failed);
}

fn run_known_issue_tests() {
    println!("  Documenting known issues:");
    println!("  1. p.sum() returns 0 instead of 30");
    println!("  2. Type inference issues with `self` keyword");
    println!("  3. Static method resolution in generic contexts");
    println!("  4. Associated functions vs methods distinction");
    println!("  5. Chained static method calls");
    println!("  These issues are documented in tests/known_issues_methods.rs");
}
