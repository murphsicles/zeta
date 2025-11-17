// benches/zeta_bench.rs: Criterion benchmarks for Zeta (semiring/add, actors, EOP algos)
use criterion::{Criterion, black_box, criterion_group, criterion_main};
use zeta::{compile_and_run_zeta, parse_zeta};

fn bench_semiring_add(c: &mut Criterion) {
    let input = r#"
fn bench_add() -> i32 {
    let mut sum = 0;
    for i in 0..1000 {
        sum = sum.add(i);
    }
    sum
}
"#;
    c.bench_function("zeta_semiring_add", |b| {
        b.iter(|| {
            let _ = black_box(compile_and_run_zeta(black_box(input)).unwrap());
        })
    });
}

fn bench_eop_matrix(c: &mut Criterion) {
    let input = r#"
concept Semiring { fn add(self: Self, rhs: Self) -> Self; fn mul(self: Self, rhs: Self) -> Self; }
impl Semiring for i32 { fn add(self: i32, rhs: i32) -> i32 { self + rhs } fn mul(self: i32, rhs: i32) -> i32 { self * rhs } }
fn matrix_mul() -> i32 { 1 } // Stub EOP
"#;
    c.bench_function("zeta_eop_matrix", |b| {
        b.iter(|| {
            let _ = black_box(compile_and_run_zeta(black_box(input)).unwrap());
        })
    });
}

criterion_group!(benches, bench_semiring_add, bench_eop_matrix);
criterion_main!(benches);
