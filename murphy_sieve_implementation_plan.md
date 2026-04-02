# Murphy's Sieve Implementation Plan

## Algorithm Overview

Murphy's Sieve uses wheel factorization with the first 6 primes:
- Wheel primes: 2, 3, 5, 7, 11, 13
- Wheel size: 30030 (2*3*5*7*11*13)
- Reduces trial divisions by approximately 77%

## Implementation Steps

### 1. Data Structures
- `Sieve` struct with dynamic bit array
- Initialize array with zeros (0 = prime, 1 = composite)
- Use `[bool; limit]` or bit array for efficiency

### 2. Wheel Factorization
- Pre-calculate residues (numbers coprime to wheel primes)
- Generate wheel pattern to skip multiples of wheel primes
- Only test numbers that are not divisible by any wheel prime

### 3. Sieve Algorithm
- Square root optimization: only check up to sqrt(limit)
- Start clearing at factor * factor
- Use while loops for clearing multiples

### 4. Prime Counting
- Count primes efficiently (skip even numbers, use wheel residues)
- Return count of primes up to 1,000,000

### 5. Benchmark Loop
- Run for at least 5 seconds
- Count iterations completed
- Output in required format: `label;iterations;total_time;num_threads;tags`

### 6. Output Tags (as per CONTRIBUTING.md)
- `algorithm=wheel`
- `faithful=yes` (no external dependencies, dynamic allocation)
- `bits=1` (one bit per flag)
- `parallel=no` (single-threaded)

## Zeta Language Considerations

### Current Limitations (to be fixed by other agents):
1. Dynamic arrays not yet implemented
2. Complex nested loops fail type checking  
3. Output tagging (`println`) not fully functional
4. Timing loops for 5-second runs not possible yet

### Once Fixed:
1. Use `[bool; limit]` for sieve storage
2. Implement proper timing with `std::time`
3. Add proper output formatting
4. Test with limit = 1,000,000

## Code Structure

```zeta
struct Sieve {
    limit: u64,
    is_prime: [bool; limit],  // Will work once arrays fixed
}

impl Sieve {
    fn new(limit: u64) -> Sieve {
        // Initialize sieve
    }
    
    fn run_sieve(&mut self) {
        // Murphy's Sieve algorithm
    }
    
    fn count_primes(&self) -> u64 {
        // Count primes
    }
}

fn main() {
    // Benchmark loop
    // Output results
}
```

## Testing Strategy

1. Verify prime count for limit=1,000,000 is 78,498
2. Test with smaller limits first (100, 1000)
3. Verify wheel factorization reduces operations
4. Benchmark against base algorithm

## Dependencies

- No external dependencies (faithful implementation)
- Only use Zeta standard library
- Compile-time calculations for wheel residues