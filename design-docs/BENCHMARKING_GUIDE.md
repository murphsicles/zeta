# Zeta Benchmarking & Performance Guide

## Overview

Zeta now has comprehensive benchmarking infrastructure to track performance and prevent regressions.

## Benchmark Types

### 1. Compiler Performance (`compiler_bench.rs`)
Measures the compiler's own performance:
- **Parse speed**: Time to parse Zeta source code
- **Type checking speed**: Semantic analysis performance
- **MIR generation speed**: Intermediate representation generation
- **Full compilation**: End-to-end compilation pipeline

### 2. Runtime Performance (`runtime_bench.rs`)
Measures execution speed of compiled Zeta code:
- **Arithmetic operations**: Basic math performance
- **Recursive functions**: Function call overhead
- **Memory operations**: Array/vector performance
- **Control flow**: If/else, loops, branches

### 3. Regression Testing (`regression_test.rs`)
Tracks performance over time and fails if performance regresses:
- **Automatic baselines**: Records performance for each commit
- **CI integration**: Fails CI if performance degrades
- **Threshold-based**: Configurable regression thresholds

## Running Benchmarks

### Quick Benchmarks (Development)
```bash
# Run all benchmarks quickly (CI-friendly)
cargo bench -- --warm-up-time 50ms --measurement-time 200ms --sample-size 10

# Run specific benchmark
cargo bench --bench compiler_bench

# Run runtime benchmarks
cargo bench --bench runtime_bench
```

### Full Benchmarks (Release)
```bash
# Run with default settings (more accurate)
cargo bench

# Generate HTML report
cargo bench -- --output-format bencher --save-baseline main
```

### Regression Testing
```bash
# Check for performance regressions
cargo run --bin regression_test

# Update performance baseline (after performance improvements)
cargo run --bin regression_test -- update
```

## CI Integration

Benchmarks are integrated into CI via `benches/run_ci_benchmarks.ps1`:

1. **Compilation check**: Ensures benchmarks compile
2. **Regression tests**: Checks for performance regressions
3. **Quick benchmarks**: Runs CI-friendly benchmarks
4. **Results reporting**: Outputs benchmark results

## Performance Baselines

Performance data is stored in `benches/performance_baseline.json`:

```json
{
  "benchmarks": {
    "parse": [125000.0, 12500.0],  // mean_ns, std_dev_ns
    "typecheck": [450000.0, 45000.0]
  },
  "commit_hash": "abc123...",
  "timestamp": "2026-03-30T10:52:00+00:00",
  "regression_threshold": 10.0
}
```

## Adding New Benchmarks

### 1. Create Benchmark File
Add a new `.rs` file in the `benches/` directory.

### 2. Update Cargo.toml
Add a new `[[bench]]` entry:
```toml
[[bench]]
name = "your_benchmark"
harness = false
```

### 3. Write Benchmark Code
Use the `criterion` crate:
```rust
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_example(c: &mut Criterion) {
    c.bench_function("example", |b| {
        b.iter(|| {
            // Code to benchmark
        });
    });
}

criterion_group!(benches, bench_example);
criterion_main!(benches);
```

## Best Practices

### For Accurate Results:
1. **Warm up**: Allow JIT compilation to complete
2. **Sufficient iterations**: Use enough samples for statistical significance
3. **Isolate variables**: Test one thing at a time
4. **Clean environment**: Close other applications during benchmarking

### For CI Benchmarks:
1. **Keep it fast**: CI has limited time
2. **Small sample sizes**: 10-20 samples is sufficient for CI
3. **Focus on regressions**: CI should catch major regressions, not measure absolute performance
4. **Fail fast**: Stop early if benchmarks don't compile

## Troubleshooting

### Common Issues:

1. **Benchmarks don't compile**:
   - Check dependencies in Cargo.toml
   - Ensure benchmark files are in `benches/` directory
   - Verify `[[bench]]` entries in Cargo.toml

2. **Performance regressions in CI**:
   - Check if code changes affected performance
   - Update baseline if regression is acceptable
   - Investigate performance bottlenecks

3. **Inconsistent results**:
   - Increase sample size
   - Extend warm-up time
   - Run on dedicated hardware

## Future Improvements

Planned benchmarking enhancements:

1. **Memory usage tracking**: Heap allocations, GC pressure
2. **Startup time**: Program initialization performance
3. **Concurrency benchmarks**: Parallel execution performance
4. **Cross-version comparison**: Compare performance across Zeta versions
5. **Automated reporting**: Generate performance reports for each release

---
**Last Updated**: 2026-03-30  
**Maintainer**: Father Zak (Benchmarking Infrastructure)