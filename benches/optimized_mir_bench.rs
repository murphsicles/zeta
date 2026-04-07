// benches/optimized_mir_bench.rs
//! Benchmark comparing original vs optimized MIR generation

use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;
use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::mir::r#gen::MirGen;
use zetac::middle::mir::optimized_gen::OptimizedMirGen;

fn bench_original_mir_gen(c: &mut Criterion) {
    let source = r#"
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
    
    fn add(a: i64, b: i64) -> i64 {
        a + b
    }
    "#;

    let (_, ast) = parse_zeta(source).unwrap();

    c.bench_function("original_mir_gen", |b| {
        b.iter(|| {
            let mut mir_gen = MirGen::new();
            for node in &ast {
                black_box(mir_gen.lower_to_mir(node));
            }
        })
    });
}

fn bench_optimized_mir_gen(c: &mut Criterion) {
    let source = r#"
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
    
    fn add(a: i64, b: i64) -> i64 {
        a + b
    }
    "#;

    let (_, ast) = parse_zeta(source).unwrap();

    c.bench_function("optimized_mir_gen", |b| {
        b.iter(|| {
            for node in &ast {
                let mut opt_gen = OptimizedMirGen::with_capacity(node);
                black_box(opt_gen.lower_to_mir(node));
            }
        })
    });
}

fn bench_mixed_workload(c: &mut Criterion) {
    let sources = [
        r#"fn simple() -> i64 { 42 }"#,
        r#"fn add(a: i64, b: i64) -> i64 { a + b }"#,
        r#"fn factorial(n: i64) -> i64 { if n <= 1 { 1 } else { n * factorial(n - 1) } }"#,
        r#"fn fibonacci(n: i64) -> i64 { if n <= 1 { n } else { fibonacci(n - 1) + fibonacci(n - 2) } }"#,
    ];

    let asts: Vec<_> = sources.iter().map(|s| parse_zeta(s).unwrap().1).collect();

    c.bench_function("original_mixed", |b| {
        b.iter(|| {
            for ast in &asts {
                let mut mir_gen = MirGen::new();
                for node in ast {
                    black_box(mir_gen.lower_to_mir(node));
                }
            }
        })
    });

    c.bench_function("optimized_mixed", |b| {
        b.iter(|| {
            for ast in &asts {
                for node in ast {
                    let mut opt_gen = OptimizedMirGen::with_capacity(node);
                    black_box(opt_gen.lower_to_mir(node));
                }
            }
        })
    });
}

criterion_group!(
    name = mir_gen_benches;
    config = Criterion::default().sample_size(20);
    targets = bench_original_mir_gen, bench_optimized_mir_gen, bench_mixed_workload
);

criterion_main!(mir_gen_benches);
