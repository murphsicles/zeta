# SIMD TEST PLAN - SIMD-TEST-AGENT

## Mission
Test SIMD implementation once compiler errors are fixed.

## Prerequisite
Wait for PARSER-ERROR-FIXER and COMPILER-FIX-AGENT to fix compilation.

## Test Tasks

### 1. Test SIMD Type Parsing
**Files**: `test_simd_parser.rs`, `test_simd_program.zeta`
**Objective**: Verify SIMD type syntax is correctly parsed
**Test Cases**:
- Shorthand syntax: `u64x8`, `f32x4`, `i32x16`
- Generic syntax: `Vector<u64, 8>`, `Vector<f32, 4>`
- With references: `&u64x8`, `&mut f32x4`
- In function signatures and variable declarations

### 2. Test SIMD Code Generation  
**Files**: `test_simd_codegen.rs`, `test_simd_working.z`
**Objective**: Verify SIMD operations generate correct LLVM IR
**Test Cases**:
- Vector type conversion to LLVM types
- SIMD splat operations
- SIMD arithmetic (add, sub, mul)
- SIMD load/store operations
- SIMD comparison operations

### 3. Test SIMD Standard Library
**Files**: `test_simd.z`, custom test files
**Objective**: Verify SIMD API works correctly
**Test Cases**:
- `std::simd` module availability
- Vector construction (`splat`, `from_array`)
- Basic operations (addition, multiplication)
- Memory operations (load, store)
- Reduction operations (sum, min, max)

### 4. Create SIMD-optimized Murphy's Sieve
**Files**: `prime_simd.z`, `test_simd_murphy.z` (to create)
**Objective**: Implement and test SIMD-optimized prime sieve
**Implementation**:
- Use SIMD for array initialization
- Use SIMD for marking multiples
- Use SIMD for prime counting
- Verify correctness against scalar implementation

### 5. Benchmark Performance Improvement
**Files**: Benchmark scripts, performance tests
**Objective**: Measure SIMD acceleration
**Metrics**:
- Execution time comparison (SIMD vs scalar)
- Speedup factor
- Memory bandwidth utilization
- Cache performance

## Test Files to Create/Use

### Existing Files:
1. `test_simd_program.zeta` - Simple SIMD test program
2. `test_simd_working.z` - Basic SIMD operations test
3. `test_simd.z` - SIMD API test
4. `prime_simd.z` - SIMD-optimized Murphy's Sieve
5. `test_simd_parser.rs` - Parser unit tests
6. `test_simd_codegen.rs` - Codegen unit tests

### Files to Create:
1. `test_simd_murphy.z` - Comprehensive SIMD sieve tests
2. `benchmark_simd.z` - Performance benchmarks
3. `test_simd_stdlib.z` - Standard library tests

## Test Execution Plan

### Phase 1: Compiler Ready (Wait for fix)
- Monitor compiler build status
- Verify `cargo build` succeeds
- Verify `cargo test` passes basic tests

### Phase 2: Basic SIMD Tests
1. Run parser tests: `cargo test test_simd_parsing`
2. Run codegen tests: `cargo test test_simd_codegen`
3. Compile simple SIMD program: `zetac test_simd_working.z`
4. Execute and verify output

### Phase 3: Standard Library Tests
1. Test SIMD module import
2. Test vector construction
3. Test basic operations
4. Test memory operations

### Phase 4: Murphy's Sieve Implementation
1. Create `test_simd_murphy.z`
2. Implement SIMD-optimized sieve
3. Test correctness (compare with scalar)
4. Optimize for performance

### Phase 5: Benchmarking
1. Create performance test harness
2. Measure SIMD vs scalar performance
3. Analyze speedup factors
4. Document results

## Success Criteria

### Minimum Success:
- SIMD types parse correctly
- Simple SIMD programs compile
- Basic SIMD operations work
- SIMD Murphy's Sieve compiles

### Target Success:
- All SIMD tests pass
- SIMD Murphy's Sieve produces correct results
- Measurable performance improvement
- Comprehensive test coverage

### Stretch Goals:
- 4-8x speedup on SIMD operations
- Full SIMD standard library working
- Production-ready SIMD optimization

## Timeline
- **21:06-21:41**: Wait for compiler fix, prepare test plan
- **21:41-22:06**: Basic SIMD parsing and codegen tests
- **22:06-22:41**: Standard library and Murphy's Sieve tests
- **22:41-23:06**: Benchmarking and performance analysis
- **23:06**: Report results

## Dependencies
1. Compiler fixes from PARSER-ERROR-FIXER
2. Compiler fixes from COMPILER-FIX-AGENT
3. SIMD implementation from other agents

## Risk Mitigation
- If compiler not fixed by 21:41, focus on test preparation
- If SIMD implementation incomplete, test available features
- If performance gains minimal, document limitations
- Always have scalar fallback for comparison

## Reporting
Final report will include:
1. Test results summary
2. Performance benchmarks
3. Issues found
4. Recommendations
5. Success/failure status