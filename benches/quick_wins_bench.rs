//! Benchmark quick performance wins
//! Measures before/after impact of optimizations

use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;
use std::time::Instant;
use zetac::frontend::parser::top_level::parse_zeta;

/// Benchmark: Float parsing performance
fn bench_float_parsing(c: &mut Criterion) {
    // Test cases with floats
    let float_cases = vec![
        ("simple_float", "let x = 3.14;"),
        (
            "multiple_floats",
            "let a = 1.0; let b = 2.5; let c = 3.14159;",
        ),
        ("float_in_expression", "let result = 1.5 + 2.5 * 3.0;"),
        ("large_float", "let big = 123456789.987654321;"),
        (
            "many_small_floats",
            "let x=1.1;let y=2.2;let z=3.3;let a=4.4;let b=5.5;",
        ),
    ];

    let mut group = c.benchmark_group("float_parsing");

    for (name, code) in float_cases {
        group.bench_function(name, |b| {
            b.iter(|| {
                let result = parse_zeta(code);
                let _ = black_box(result);
            });
        });
    }

    group.finish();
}

/// Benchmark: Integer vs Float parsing comparison
fn bench_number_parsing_comparison(c: &mut Criterion) {
    let int_code = "let x = 42; let y = 100; let z = 999;";
    let float_code = "let x = 42.0; let y = 100.5; let z = 999.99;";

    let mut group = c.benchmark_group("number_parsing");

    group.bench_function("integers", |b| {
        b.iter(|| {
            let result = parse_zeta(int_code);
            let _ = black_box(result);
        });
    });

    group.bench_function("floats", |b| {
        b.iter(|| {
            let result = parse_zeta(float_code);
            let _ = black_box(result);
        });
    });

    group.finish();
}

/// Benchmark: File parsing with mixed content
fn bench_file_parsing(c: &mut Criterion) {
    let test_files = vec![
        "test_simple_const.z",
        "test_static_method_integration.z",
        "tests/smoke_test.z",
    ];

    let mut group = c.benchmark_group("file_parsing");

    for file in test_files {
        if let Ok(content) = std::fs::read_to_string(file) {
            group.bench_function(file, |b| {
                b.iter(|| {
                    let result = parse_zeta(&content);
                    let _ = black_box(result);
                });
            });
        }
    }

    group.finish();
}

/// Manual timing for quick comparison
#[allow(dead_code)]
fn quick_timing_test() {
    println!("=== QUICK TIMING TEST (Float Parser Optimization) ===");
    println!();

    let test_cases = vec![
        ("Simple float", "let x = 3.14;"),
        (
            "Multiple floats",
            "let a = 1.0; let b = 2.5; let c = 3.14159;",
        ),
        ("Float expression", "let result = 1.5 + 2.5 * 3.0;"),
    ];

    for (name, code) in test_cases {
        // Warm up
        for _ in 0..100 {
            let _ = parse_zeta(code);
        }

        // Time 1000 iterations
        let start = Instant::now();
        let iterations = 1000;
        for _ in 0..iterations {
            let _ = parse_zeta(code);
        }
        let duration = start.elapsed();

        let per_iteration = duration.as_nanos() as f64 / iterations as f64;
        println!("{}: {:.2} ns/iteration", name, per_iteration);
    }

    println!();
    println!("Note: Run 'cargo bench --bench quick_wins_bench' for accurate measurements");
}

// Configure benchmark groups
criterion_group!(
    name = quick_wins;
    config = Criterion::default()
        .sample_size(100)  // More samples for accuracy
        .warm_up_time(std::time::Duration::from_millis(100))
        .measurement_time(std::time::Duration::from_millis(500));
    targets = bench_float_parsing, bench_number_parsing_comparison, bench_file_parsing
);

criterion_main!(quick_wins);

// Also provide a simple main for quick testing
// Note: This is commented out because criterion_main! already defines main
/*
fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 && args[1] == "quick" {
        quick_timing_test();
    } else {
        // Run criterion benchmarks
        let mut criterion = Criterion::default()
            .sample_size(50)
            .warm_up_time(std::time::Duration::from_millis(50))
            .measurement_time(std::time::Duration::from_millis(200));

        let mut group = criterion.benchmark_group("quick");
        bench_float_parsing(&mut group);
        group.finish();
    }
}
*/
