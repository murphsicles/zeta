# Zeta Benchmark Performance Report

## Summary

- **Generated**: 2026-04-05 15:09:39
- **Total Benchmarks**: 2
- **Total Iterations**: 200
- **Total Execution Time**: 19261 ms
- **Average Success Rate**: 91.5%

## Individual Benchmark Results

### comparison_framework

- **File**: C:\Users\mummy\.openclaw\workspace\benchmarks\comparison_framework.z
- **Timestamp**: 2026-04-05T15:09:39
- **Iterations**: 100
- **Completed**: 91
- **Success Rate**: 91.0%
- **Min Time**: 80.00 ms
- **Max Time**: 128.00 ms
- **Average Time**: 105.00 ms
- **Median Time**: 108.00 ms
- **Throughput**: 9.52 ops/sec
- **Total Time**: 9555.00 ms

### murphy_sieve_performance_benchmark

- **File**: C:\Users\mummy\.openclaw\workspace\benchmarks\murphy_sieve_performance_benchmark.z
- **Timestamp**: 2026-04-05T15:09:39
- **Iterations**: 100
- **Completed**: 92
- **Success Rate**: 92.0%
- **Min Time**: 80.00 ms
- **Max Time**: 129.00 ms
- **Average Time**: 105.50 ms
- **Median Time**: 108.00 ms
- **Throughput**: 9.48 ops/sec
- **Total Time**: 9706.00 ms

## Performance Comparison

| Benchmark | Avg Time (ms) | Throughput (ops/sec) | Success Rate |
|-----------|---------------|----------------------|--------------|
| comparison_framework | 105.00 | 9.52 | 91.0% |
| murphy_sieve_performance_benchmark | 105.50 | 9.48 | 92.0% |

## Recommendations

1. **comparison_framework** is 1.0x faster than **murphy_sieve_performance_benchmark**
2. Consider optimizing **murphy_sieve_performance_benchmark** implementation
3. **comparison_framework** has low success rate (91.0%) - investigate stability
4. Run with -Quick flag for development, full iterations for release testing
5. Use -Filter parameter to run specific benchmarks

