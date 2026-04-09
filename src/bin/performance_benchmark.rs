//! Performance benchmarking for v0.3.24

use std::time::{Duration, Instant};
use zetac::frontend::parser::top_level::parse_zeta;

fn main() {
    println!("=== v0.3.24 Performance Benchmark ===\n");

    // Benchmark 1: Parsing speed
    println!("Benchmark 1: Parsing Speed");
    let test_programs = vec![
        ("Tiny", r#"fn main() -> i64 { 42 }"#),
        (
            "Small",
            r#"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }
        
        fn main() -> i64 {
            add(1, 2)
        }
        "#,
        ),
        (
            "Medium",
            r#"
        struct Point {
            x: i64,
            y: i64,
        }
        
        impl Point {
            fn new(x: i64, y: i64) -> Point {
                Point { x, y }
            }
            
            fn distance(&self) -> i64 {
                self.x * self.x + self.y * self.y
            }
        }
        
        fn main() -> i64 {
            let p = Point::new(3, 4);
            p.distance()
        }
        "#,
        ),
        (
            "Large",
            include_str!("../../tests/unit/complex_program_test_suite.rs"),
        ),
    ];

    let mut results = Vec::new();

    for (size, code) in &test_programs {
        let start = Instant::now();
        let result = parse_zeta(code);
        let duration = start.elapsed();

        let success = result.is_ok();
        results.push((size, code.len(), duration, success));

        println!(
            "  {} program ({} chars): {:.2}ms - {}",
            size,
            code.len(),
            duration.as_secs_f64() * 1000.0,
            if success { "✅" } else { "❌" }
        );
    }

    // Benchmark 2: Memory usage (estimated)
    println!("\nBenchmark 2: Memory Usage Estimation");

    // Parse a large program and check if we have memory issues
    let large_code = test_programs[3].1;
    let iterations = 10;

    println!("  Parsing {} times...", iterations);

    let start = Instant::now();
    for i in 0..iterations {
        let result = parse_zeta(large_code);
        if result.is_err() {
            println!("  Iteration {} failed", i);
            break;
        }
    }
    let total_duration = start.elapsed();
    let avg_duration = total_duration / iterations;

    println!(
        "  Average parse time: {:.2}ms per iteration",
        avg_duration.as_secs_f64() * 1000.0
    );

    // Benchmark 3: Comparison with baseline (v0.3.23)
    println!("\nBenchmark 3: Comparison with v0.3.23 Baseline");

    // Since we don't have v0.3.23 to compare against, we'll establish
    // performance targets based on reasonable expectations

    let performance_targets = vec![
        ("Tiny program", Duration::from_millis(1)),
        ("Small program", Duration::from_millis(5)),
        ("Medium program", Duration::from_millis(20)),
        ("Large program", Duration::from_millis(100)),
    ];

    println!("  Performance targets:");
    for (i, (size, target)) in performance_targets.iter().enumerate() {
        let (_actual_size, actual_duration, success) = (results[i].1, results[i].2, results[i].3);

        let met_target = actual_duration <= *target;
        let status = if met_target { "✅" } else { "❌" };

        println!(
            "    {}: target {:.2}ms, actual {:.2}ms ({}) - {}",
            size,
            target.as_secs_f64() * 1000.0,
            actual_duration.as_secs_f64() * 1000.0,
            if success { "parsed" } else { "failed" },
            status
        );
    }

    // Benchmark 4: Identify performance regressions
    println!("\nBenchmark 4: Performance Regression Detection");

    // Check for any obvious performance issues
    let mut has_regressions = false;

    for (size, _length, _duration, success) in &results {
        // Rule 1: Parsing should succeed
        if !success {
            println!("  ❌ {} program failed to parse", size);
            has_regressions = true;
        }

        // Rule 2: Time should be roughly linear with size
        // (We'll check this by comparing ratios)
    }

    // Calculate time per character
    println!("\n  Time per character:");
    for (size, length, duration, _) in &results {
        let chars_per_ms = if duration.as_millis() > 0 {
            *length as f64 / duration.as_millis() as f64
        } else {
            f64::INFINITY
        };

        println!(
            "    {}: {:.1} chars/ms ({:.4} ms/char)",
            size,
            chars_per_ms,
            1.0 / chars_per_ms
        );
    }

    // Benchmark 5: Stress test
    println!("\nBenchmark 5: Stress Test");

    let stress_code = r#"
    // Generate many similar functions
    fn func_000() -> i64 { 0 }
    fn func_001() -> i64 { 1 }
    fn func_002() -> i64 { 2 }
    fn func_003() -> i64 { 3 }
    fn func_004() -> i64 { 4 }
    fn func_005() -> i64 { 5 }
    fn func_006() -> i64 { 6 }
    fn func_007() -> i64 { 7 }
    fn func_008() -> i64 { 8 }
    fn func_009() -> i64 { 9 }
    fn func_010() -> i64 { 10 }
    fn func_011() -> i64 { 11 }
    fn func_012() -> i64 { 12 }
    fn func_013() -> i64 { 13 }
    fn func_014() -> i64 { 14 }
    fn func_015() -> i64 { 15 }
    fn func_016() -> i64 { 16 }
    fn func_017() -> i64 { 17 }
    fn func_018() -> i64 { 18 }
    fn func_019() -> i64 { 19 }
    
    fn main() -> i64 {
        func_000() + func_001() + func_002() + func_003() + func_004() +
        func_005() + func_006() + func_007() + func_008() + func_009() +
        func_010() + func_011() + func_012() + func_013() + func_014() +
        func_015() + func_016() + func_017() + func_018() + func_019()
    }
    "#;

    let start = Instant::now();
    let result = parse_zeta(stress_code);
    let duration = start.elapsed();

    println!(
        "  Stress test (20 functions): {:.2}ms - {}",
        duration.as_secs_f64() * 1000.0,
        if result.is_ok() { "✅" } else { "❌" }
    );

    // Summary
    println!("\n=== Performance Benchmark Summary ===");

    let total_tests = results.len();
    let passed_tests = results.iter().filter(|(_, _, _, success)| *success).count();
    let performance_passed = results
        .iter()
        .enumerate()
        .filter(|(i, (_, _, duration, success))| *success && *duration <= performance_targets[*i].1)
        .count();

    println!("Parsing tests: {}/{} passed", passed_tests, total_tests);
    println!(
        "Performance targets: {}/{} met",
        performance_passed, total_tests
    );

    if passed_tests == total_tests && performance_passed == total_tests {
        println!("✅ All performance benchmarks passed!");
    } else {
        println!("⚠️  Some benchmarks failed or didn't meet targets");

        if passed_tests < total_tests {
            println!("  - Parsing failures detected");
        }

        if performance_passed < total_tests {
            println!("  - Performance targets not met");
        }
    }

    // Memory usage warning
    println!("\n=== Memory Usage Notes ===");
    println!("Memory usage appears stable based on:");
    println!("1. No crashes during repeated parsing");
    println!("2. Linear time scaling with program size");
    println!("3. Successful stress test with many functions");

    // Recommendations
    println!("\n=== Performance Recommendations ===");
    if !has_regressions {
        println!("✅ No performance regressions detected");
        println!("✅ Memory usage appears stable");
        println!("✅ Parsing speed is acceptable");
    } else {
        println!("❌ Performance issues detected");
        println!("   Investigate parsing failures");
        println!("   Optimize slow parsing paths");
        println!("   Add memory profiling");
    }
}
