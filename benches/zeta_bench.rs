// benches/zeta_bench.rs
#![allow(deprecated)]

use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;
use zeta::compile_and_run_zeta;

fn bench_semiring_add(c: &mut Criterion) {
    let input = r#"
concept Addable<Rhs=Self> {
    fn add(self: Self, rhs: Rhs) -> Self;
}
impl Addable for i32 {
    fn add(self: i32, rhs: i32) -> i32;
}
fn semiring_add() -> i32 {
    let mut sum = 0i32;
    for i in 0..1000 {
        sum = sum.add(i as i32);
    }
    sum
}
"#;
    c.bench_function("zeta_semiring_add", |b| {
        b.iter(|| black_box(compile_and_run_zeta(black_box(input)).unwrap()))
    });
}

fn bench_eop_matrix(c: &mut Criterion) {
    let input = r#"
concept Semiring {
    fn add(self: Self, rhs: Self) -> Self;
    fn mul(self: Self, rhs: Self) -> Self;
}
impl Semiring for i32 {
    fn add(self: i32, rhs: i32) -> i32 { self + rhs }
    fn mul(self: i32, rhs: i32) -> i32 { self * rhs }
}
fn eop_matrix_mul(n: i32) -> i32 {
    let mut res = 1i32;
    for _ in 0..n {
        res = res.mul(2);
    }
    res
}
"#;
    c.bench_function("zeta_eop_matrix", |b| {
        b.iter(|| black_box(compile_and_run_zeta(black_box(input)).unwrap()))
    });
}

fn bench_actor_counter(c: &mut Criterion) {
    let input = r#"
actor Counter {
    async fn increment(&self, delta: i32) -> i32;
}
impl Send for Counter {}
impl Sync for Counter {}
impl CacheSafe for Counter {}
fn actor_bench() -> i32 {
    let c = spawn_actor Counter();
    c.increment(1);
    42
}
"#;
    c.bench_function("zeta_actor_counter", |b| {
        b.iter(|| black_box(compile_and_run_zeta(black_box(input)).unwrap()))
    });
}

fn bench_vs_rust(c: &mut Criterion) {
    // Stub Rust baseline (external compile)
    let rust_time = 0.5f64; // Placeholder: run `cargo bench` on Rust equiv
    c.bench_function("rust_baseline_add", |b| b.iter(|| black_box(rust_time)));
}

criterion_group!(
    benches,
    bench_semiring_add,
    bench_eop_matrix,
    bench_actor_counter,
    bench_vs_rust
);
criterion_main!(benches);
