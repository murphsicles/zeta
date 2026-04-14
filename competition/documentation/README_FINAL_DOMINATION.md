# FINAL DOMINATION - Murphy's Sieve with sqrt Optimization

## Algorithm
- Murphy's Sieve (trial division) implementation
- Optimized with sqrt(n) bound using `d > n / d` comparison
- Avoids multiplication (`d * d <= n`) which could overflow
- Mathematically equivalent: `d > n / d` ⇔ `d * d > n`

## Features
1. **Correct sqrt optimization** without buggy comparisons
2. Uses `d > n / d` break condition (mathematically correct)
3. Returns **78498** for limit=1,000,000 (verified)
4. Competition-ready infinite loop wrapper
5. Only checks odd numbers and odd divisors
6. Handles edge cases (limit < 2, limit < 3)

## Files
- `FINAL_DOMINATION.z` - Main competition entry with infinite loop
- Algorithm computes prime count for 1,000,000 in each iteration
- Competition harness runs for 5 seconds, counts passes

## Verification
- Primes ≤ 30: 2,3,5,7,11,13,17,19,23,29 = 10 primes ✓
- Primes ≤ 1,000,000: 78,498 primes ✓

## Competition Usage
```bash
zeta FINAL_DOMINATION.z
```
Program runs in infinite loop, each iteration returns 78498.
Competition measures passes in 5-second window.

## Optimization Details
- Skips even numbers > 2 (all composite)
- Skips even divisors > 2 (can't divide odd numbers)
- Uses integer division `n / d` instead of multiplication
- Early break when `d > n / d` (equivalent to `d > sqrt(n)`)

## Time Complexity
- O(n√n) worst case
- Practical for competition limits (1,000,000)