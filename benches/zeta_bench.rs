//! Minimal benchmark for Zeta compiler
//!
//! This is a placeholder benchmark to satisfy Cargo.toml configuration.
//! Proper benchmarks will be implemented when the compiler is more stable.

use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

/// Minimal benchmark that does nothing
fn bench_minimal(c: &mut Criterion) {
    c.bench_function("minimal", |b| {
        b.iter(|| {
            // Do nothing - placeholder benchmark
            black_box(());
        });
    });
}

// Configure and run benchmarks
criterion_group!(
    name = benches;
    config = Criterion::default()
        .sample_size(10)  // Very small for CI
        .warm_up_time(std::time::Duration::from_millis(10))
        .measurement_time(std::time::Duration::from_millis(50));
    targets = bench_minimal
);

criterion_main!(benches);
