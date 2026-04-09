//! Compiler performance benchmarks for Zeta
//!
//! Measures:
//! 1. Parse speed
//! 2. Type checking speed  
//! 3. Code generation speed
//! 4. Full compilation pipeline

use criterion::{Criterion, criterion_group, criterion_main};
use std::fs;
use std::hint::black_box;
use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::resolver::Resolver;

/// Test Zeta source code for benchmarking
const TEST_SOURCE: &str = r#"
// Simple arithmetic benchmark
fn add(a: i64, b: i64) -> i64 {
    a + b
}

fn factorial(n: i64) -> i64 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn fibonacci(n: i64) -> i64 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

fn main() -> i64 {
    let x = add(10, 20);
    let y = factorial(5);
    let z = fibonacci(10);
    x + y + z
}
"#;

/// Benchmark: Parse speed
fn bench_parse(c: &mut Criterion) {
    c.bench_function("parse", |b| {
        b.iter(|| {
            let result = parse_zeta(TEST_SOURCE);
            let _ = black_box(result);
        });
    });
}

/// Benchmark: Type checking speed
fn bench_type_check(c: &mut Criterion) {
    c.bench_function("type_check", |b| {
        b.iter(|| {
            let (_, asts) = parse_zeta(TEST_SOURCE).unwrap();
            let mut resolver = Resolver::new();
            for ast in &asts {
                resolver.register(ast.clone());
            }
            let ok = resolver.typecheck(&asts);
            let _ = black_box(ok);
        });
    });
}

/// Benchmark: MIR generation speed
fn bench_mir_generation(c: &mut Criterion) {
    c.bench_function("mir_generation", |b| {
        b.iter(|| {
            let (_, asts) = parse_zeta(TEST_SOURCE).unwrap();
            let mut resolver = Resolver::new();
            for ast in &asts {
                resolver.register(ast.clone());
            }
            resolver.typecheck(&asts);

            // Collect function ASTs and generate MIR
            let func_asts: Vec<_> = asts
                .iter()
                .filter_map(|ast| {
                    if let zetac::frontend::ast::AstNode::FuncDef { name, .. } = ast {
                        Some((name.clone(), ast))
                    } else {
                        None
                    }
                })
                .collect();

            for (_, ast) in func_asts {
                let mir = resolver.lower_to_mir(ast);
                let _ = black_box(mir);
            }
        });
    });
}

/// Benchmark: Full compilation pipeline
fn bench_full_compilation(c: &mut Criterion) {
    c.bench_function("full_compilation", |b| {
        b.iter(|| {
            let (_, asts) = parse_zeta(TEST_SOURCE).unwrap();
            let mut resolver = Resolver::new();
            for ast in &asts {
                resolver.register(ast.clone());
            }

            if !resolver.typecheck(&asts) {
                return;
            }

            // Generate MIR for all functions
            let func_asts: Vec<_> = asts
                .iter()
                .filter_map(|ast| {
                    if let zetac::frontend::ast::AstNode::FuncDef { name, .. } = ast {
                        Some((name.clone(), ast))
                    } else {
                        None
                    }
                })
                .collect();

            let mut mir_map = std::collections::HashMap::new();
            for (name, ast) in func_asts {
                let mir = resolver.lower_to_mir(ast);
                mir_map.insert(name, mir);
            }

            let _ = black_box(mir_map);
        });
    });
}

/// Benchmark: File parsing (real Zeta source files)
fn bench_file_parsing(c: &mut Criterion) {
    // Look for Zeta source files in the workspace
    let test_files = vec![
        "test_simple_const.z",
        "test_static_method_integration.z",
        "tests/smoke_test.z",
    ];

    let mut group = c.benchmark_group("file_parsing");

    for file in test_files {
        if let Ok(content) = fs::read_to_string(file) {
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

// Configure benchmark groups
criterion_group!(
    name = compiler_benches;
    config = Criterion::default()
        .sample_size(20)
        .warm_up_time(std::time::Duration::from_millis(100))
        .measurement_time(std::time::Duration::from_millis(500));
    targets = bench_parse, bench_type_check, bench_mir_generation, bench_full_compilation
);

criterion_group!(
    name = file_benches;
    config = Criterion::default()
        .sample_size(10)
        .warm_up_time(std::time::Duration::from_millis(50))
        .measurement_time(std::time::Duration::from_millis(200));
    targets = bench_file_parsing
);

criterion_main!(compiler_benches, file_benches);
