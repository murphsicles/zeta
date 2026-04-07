// benches/typecheck_bench.rs
//! Benchmark comparing original vs cached type checking

use criterion::{Criterion, criterion_group, criterion_main};
use zetac::frontend::parser::top_level::parse_zeta;
use zetac::middle::resolver::resolver::Resolver;
use zetac::middle::resolver::type_cache::CachingTypeChecker;

fn bench_original_typecheck(c: &mut Criterion) {
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
    
    fn multiply(a: i64, b: i64) -> i64 {
        a * b
    }
    
    fn subtract(a: i64, b: i64) -> i64 {
        a - b
    }
    "#;

    let (_, ast) = parse_zeta(source).unwrap();

    c.bench_function("original_typecheck", |b| {
        b.iter(|| {
            let mut resolver = Resolver::new();
            std::hint::black_box(resolver.typecheck(&ast));
        })
    });
}

fn bench_cached_typecheck(c: &mut Criterion) {
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
    
    fn multiply(a: i64, b: i64) -> i64 {
        a * b
    }
    
    fn subtract(a: i64, b: i64) -> i64 {
        a - b
    }
    "#;

    let (_, ast) = parse_zeta(source).unwrap();

    c.bench_function("cached_typecheck", |b| {
        b.iter(|| {
            let resolver = Resolver::new();
            let mut cached_checker = CachingTypeChecker::new(resolver);
            std::hint::black_box(cached_checker.typecheck(&ast));
        })
    });
}

fn bench_repeated_typecheck(c: &mut Criterion) {
    let source = r#"
    fn simple() -> i64 { 42 }
    fn add(a: i64, b: i64) -> i64 { a + b }
    "#;

    let (_, ast) = parse_zeta(source).unwrap();

    c.bench_function("original_repeated", |b| {
        b.iter(|| {
            for _ in 0..10 {
                let mut resolver = Resolver::new();
                std::hint::black_box(resolver.typecheck(&ast));
            }
        })
    });

    c.bench_function("cached_repeated", |b| {
        b.iter(|| {
            let resolver = Resolver::new();
            let mut cached_checker = CachingTypeChecker::new(resolver);
            for _ in 0..10 {
                std::hint::black_box(cached_checker.typecheck(&ast));
            }
        })
    });
}

criterion_group!(
    name = typecheck_benches;
    config = Criterion::default().sample_size(20);
    targets = bench_original_typecheck, bench_cached_typecheck, bench_repeated_typecheck
);

criterion_main!(typecheck_benches);
