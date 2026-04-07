# Murphy's Sieve with Const Generics - Competition Submission

## Overview
This submission implements Murphy's Sieve (Sieve of Eratosthenes) using const generics in the Zeta programming language. The implementation showcases:
1. Compile-time computation with const generics
2. SIMD-optimized version for performance
3. Benchmark comparisons
4. Ready-to-use competition entry

## Files

### 1. `murphy_sieve_const_generics.z`
- Basic implementation using const generics
- Returns prime sieve as `[bool; LIMIT]` array
- Compile-time evaluation for maximum performance

### 2. `murphy_sieve_simd_const_generics.z`
- SIMD-optimized version
- Better cache locality and vectorization
- Same const generics interface

### 3. `const_generics_benchmark.z`
- Performance benchmarking suite
- Compares different sieve implementations
- Measures compile-time vs runtime performance

## Usage

### Basic Usage
```zeta
// When const generics are implemented:
// comptime fn sieve<const LIMIT: usize>() -> [bool; LIMIT]
let primes = sieve::<1000000>();
```

### Current Workaround
```zeta
// Current implementation (fixed size):
let primes = sieve(); // Returns [bool; 1000000]
```

## Performance Characteristics

1. **Compile-time Evaluation**: The sieve is computed at compile time when possible
2. **Memory Efficiency**: Uses boolean array (1 byte per number)
3. **Time Complexity**: O(n log log n) for Sieve of Eratosthenes
4. **Space Complexity**: O(n) for storing sieve results

## Benchmark Results
(To be filled when const generics are implemented)

## Competition Goals
- Top 3 submission using proper const generics
- Demonstrate Zeta's compile-time capabilities
- Showcase performance optimizations
- Provide reusable library code

## Notes
- Currently uses fixed size as placeholder until const generics are fully implemented
- Ready to switch to `sieve<const LIMIT: usize>()` syntax when available
- Includes both basic and SIMD-optimized versions