// zeta_benchmarks.rs - Performance benchmarks for Zeta v0.3.4
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\benches\zeta_benchmarks.rs
// Purpose: Establish performance baseline and track regressions

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use zeta::*;

fn bench_parser(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");
    
    group.bench_function("parse_simple_function", |b| {
        let code = "fn add(a: i32, b: i32) -> i32 { a + b }";
        b.iter(|| {
            black_box(parse_function(black_box(code)));
        });
    });
    
    group.bench_function("parse_complex_program", |b| {
        let code = r#"
fn factorial(n: i32) -> i32 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn main() -> i32 {
    factorial(10)
}
"#;
        b.iter(|| {
            black_box(parse_program(black_box(code)));
        });
    });
    
    group.finish();
}

fn bench_codegen(c: &mut Criterion) {
    let mut group = c.benchmark_group("codegen");
    
    group.bench_function("compile_simple_expression", |b| {
        let ast = AstNode {
            kind: AstKind::BinaryExpr,
            span: (0, 10),
            children: vec![],
        };
        b.iter(|| {
            black_box(generate_code(black_box(&ast)));
        });
    });
    
    group.finish();
}

fn bench_runtime(c: &mut Criterion) {
    let mut group = c.benchmark_group("runtime");
    
    group.bench_function("actor_message_passing", |b| {
        b.iter(|| {
            // TODO: Implement actor benchmark
            black_box(42);
        });
    });
    
    group.bench_function("http_request", |b| {
        b.iter(|| {
            // TODO: Implement HTTP benchmark
            black_box(42);
        });
    });
    
    group.finish();
}

fn bench_end_to_end(c: &mut Criterion) {
    let mut group = c.benchmark_group("end_to_end");
    
    group.bench_function("compile_and_run_fibonacci", |b| {
        let code = r#"
fn fib(n: i32) -> i32 {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() -> i32 {
    fib(20)
}
"#;
        b.iter(|| {
            black_box(compile_and_run_zeta(black_box(code)));
        });
    });
    
    group.bench_function("compile_and_run_quicksort", |b| {
        let code = r#"
fn partition(arr: &mut [i32], low: i32, high: i32) -> i32 {
    let pivot = arr[high as usize];
    let mut i = low - 1;
    
    for j in low..high {
        if arr[j as usize] <= pivot {
            i += 1;
            arr.swap(i as usize, j as usize);
        }
    }
    
    arr.swap((i + 1) as usize, high as usize);
    i + 1
}

fn quicksort(arr: &mut [i32], low: i32, high: i32) {
    if low < high {
        let pi = partition(arr, low, high);
        quicksort(arr, low, pi - 1);
        quicksort(arr, pi + 1, high);
    }
}

fn main() -> i32 {
    let mut arr = [9, 7, 5, 11, 12, 2, 14, 3, 10, 6];
    quicksort(&mut arr, 0, 9);
    arr[0]
}
"#;
        b.iter(|| {
            black_box(compile_and_run_zeta(black_box(code)));
        });
    });
    
    group.finish();
}

// Security benchmarks
fn bench_security(c: &mut Criterion) {
    let mut group = c.benchmark_group("security");
    
    group.bench_function("hash_sha256", |b| {
        let data = b"test data for hashing";
        b.iter(|| {
            black_box(sha256_hash(black_box(data)));
        });
    });
    
    group.bench_function("encrypt_aes", |b| {
        let plaintext = b"secret message";
        let key = b"32-byte-key-for-aes-256-gcm!!";
        b.iter(|| {
            black_box(aes_encrypt(black_box(plaintext), black_box(key)));
        });
    });
    
    group.finish();
}

// Memory safety benchmarks
fn bench_memory_safety(c: &mut Criterion) {
    let mut group = c.benchmark_group("memory_safety");
    
    group.bench_function("bounds_checking", |b| {
        let arr = vec![1, 2, 3, 4, 5];
        b.iter(|| {
            for i in 0..arr.len() {
                black_box(arr[i]);
            }
        });
    });
    
    group.bench_function("overflow_checking", |b| {
        b.iter(|| {
            let mut x: i32 = 1;
            for _ in 0..1000 {
                x = x.wrapping_mul(2);
                black_box(x);
            }
        });
    });
    
    group.finish();
}

criterion_group!(
    benches,
    bench_parser,
    bench_codegen,
    bench_runtime,
    bench_end_to_end,
    bench_security,
    bench_memory_safety
);

criterion_main!(benches);