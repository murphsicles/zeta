# PrimeZeta Solution - Murphy's Sieve Implementation

## zeta solution by murphsicles

**algorithm=wheel**  
**faithful=yes**  
**bits=1**  
**parallel=no**

## Overview

This is a submission for the **Plummers Prime Drag Race** competition in the **PrimeZeta** category. The implementation features Murphy's Sieve with wheel factorization optimization, written entirely in pure Zeta.

## Algorithm Details

### Murphy's Sieve with Wheel Factorization
- **Wheel primes**: 2, 3, 5, 7, 11, 13
- **Wheel size**: 30030 (2×3×5×7×11×13)
- **Optimization**: Reduces trial divisions by approximately 77%

### Key Features
1. **Pure Zeta implementation** - No external dependencies
2. **Wheel factorization** - Skips multiples of first 6 primes
3. **Memory efficient** - 1 bit per number flag
4. **Faithful implementation** - Follows exact algorithm specification

## Performance

- **Benchmark duration**: 5 seconds
- **Output format**: `zeta;iterations;total_time;1;algorithm=wheel;faithful=yes;bits=1`
- **Prime count verification**: 78,498 primes up to 1,000,000

## Build Instructions

### Using Docker (Recommended)
```bash
docker build -t primezeta .
docker run primezeta
```

### Manual Build
1. Build Zeta compiler:
   ```bash
   cargo build --release --bin zetac
   ```

2. Compile Murphy's Sieve:
   ```bash
   ./target/release/zetac src/prime.z -o prime.bc
   clang prime.bc -o prime_zeta
   ```

3. Build benchmark runner:
   ```bash
   rustc src/prime_benchmark.rs -o prime_benchmark
   ```

4. Run benchmark:
   ```bash
   ./prime_benchmark
   ```

## Verification

The submission includes verification scripts:
- `test_benchmark` - Validates output format
- `test_prime_count` - Verifies prime count (78,498)

## Implementation Notes

This implementation uses Zeta's unique capabilities:
- **Algebraic semiring CTFE** for compile-time optimizations
- **CacheSafe memory model** for maximum performance
- **Thin monomorphization** for minimal binary size
- **Built-in UTF-8 string literals** for efficient output

## License

MIT License - See LICENSE file for details.

## Author

Dr. Roy Murphy (murphsicles)