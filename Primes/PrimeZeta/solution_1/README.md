# Murphy's Sieve - PrimeZeta Competition Submission

## Solution Details

**Algorithm**: Murphy's Sieve with 30030-wheel optimization  
**Faithful**: yes  
**Bits per candidate**: 1  
**Parallel**: no

**Tags**: algorithm=wheel, faithful=yes, bits=1, parallel=no  

## Implementation

This submission implements Murphy's Sieve, a highly optimized prime counting algorithm that uses:

1. **Bit-packed arrays**: 1 bit per odd number candidate
2. **30030-wheel optimization**: Eliminates multiples of 2, 3, 5, 7, 11, 13 (77% of candidates)
3. **Segment processing**: Processes in 32KB segments for L1 cache efficiency
4. **Incremental marking**: Marks only odd multiples using p² + 2kp pattern

## Performance Characteristics

- **Time Complexity**: O(n log log n)
- **Space Complexity**: O(n/2) bits (bit-packed)
- **Wheel optimization**: 30030-wheel reduces candidate count by ~77%
- **Cache efficiency**: 32KB segment processing fits in L1 cache

## Competition Format

The submission follows the PrimeZeta competition requirements:

1. **Infinite loop wrapper**: Runs continuously for 5-second benchmark
2. **Output format**: Prints prime count (78,498 for limit=1,000,000)
3. **Verification**: Returns mathematically correct result
4. **Pure Zeta**: No external dependencies or Rust code

## File Structure

```
Primes/PrimeZeta/solution_1/
├── README.md              # This file
└── src/
    └── prime.z           # Murphy's Sieve implementation
```

## Building and Running

The implementation is pure Zeta code and should compile with any compliant Zeta compiler.

## Verification

The algorithm has been mathematically verified to produce:
- 4 primes ≤ 10
- 25 primes ≤ 100  
- 168 primes ≤ 1,000
- 1,229 primes ≤ 10,000
- 9,592 primes ≤ 100,000
- 78,498 primes ≤ 1,000,000

## Author

Roy Murphy (murphsicles)