# 30030-Wheel Algorithm Zeta Implementation Plan

## Goal
Port the C implementation (`INTEGRATED_COMPETITION_MAX.c`) that achieves 12,636 passes/5s to pure Zeta code for competition domination.

## Current C Implementation Features
1. **Bit arrays**: u64 bit packing (64x memory efficiency)
2. **SIMD/AVX-512**: Vector operations for performance
3. **30030-wheel**: φ(30030)=5760 residues optimization
4. **Cache alignment**: 64-byte aligned memory
5. **Loop unrolling**: 8x for prime 3, 4x for inner loops
6. **Branch prediction**: likely/unlikely hints
7. **Prefetching**: Hardware prefetch hints

## Zeta Implementation Requirements

### 1. Bit Array Infrastructure
- Need bit manipulation operations in Zeta
- `bit_set`, `bit_test`, `bit_clear` functions
- Population count (`popcnt`) intrinsic

### 2. SIMD Support
- AVX-512 vector types in Zeta
- SIMD intrinsics or compiler auto-vectorization
- Cache line alignment support

### 3. CTFE (Compile-Time Function Evaluation)
- Generate residue tables at compile time
- Pre-compute wheel stepping patterns
- Embed constants in binary

### 4. Memory Management
- Heap allocation for large arrays
- Aligned memory allocation
- Proper deallocation

### 5. Optimization Annotations
- `#[inline]` or `inline` keyword
- Branch prediction hints
- Loop unrolling pragmas

## Implementation Phases

### Phase 1: Core Algorithm (Basic Working Version)
- Implement basic bit array sieve
- Get correct prime count (78498)
- Work within current Zeta limitations

### Phase 2: 30030-Wheel Optimization
- Add wheel factorization
- Implement residue skipping
- Optimize memory access patterns

### Phase 3: Performance Optimizations
- Add SIMD operations
- Implement cache alignment
- Add loop unrolling
- Add branch prediction hints

### Phase 4: Competition Integration
- Infinite loop with println output
- Competition format: `author;passes;time;num_threads;tags`
- Docker container setup

## Current Zeta Limitations to Address
1. **Heap allocation**: Large arrays need heap allocation
2. **SIMD types**: Need AVX-512 vector types
3. **Bit operations**: Need bit manipulation intrinsics
4. **CTFE arrays**: Need compile-time array generation
5. **Comparison operators**: Need to fix `<`, `<=` parsing

## Immediate Action Items
1. Create basic Zeta version of Murphy's Sieve with bit arrays
2. Test with limit=1,000,000 (should return 78498)
3. Fix any compiler issues (parser, type checker, runtime)
4. Add 30030-wheel optimization
5. Benchmark and optimize

## Success Criteria
- Pure Zeta code (no C/Rust dependencies)
- Returns correct prime count (78498)
- Beats C #1 benchmark (12,451 passes/5s)
- Follows competition format exactly
- Docker submission ready