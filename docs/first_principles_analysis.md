# FIRST PRINCIPLES ANALYSIS

## PROBLEM: Count primes ≤ 1,000,000 without arrays

## MATHEMATICAL REPRESENTATION:

### Option 1: Integer as bit array (64 bits)
- Segment size = 64 numbers
- Process 15,625 segments (1,000,000 / 64)
- For each segment, use u64 as bitmask
- Mark multiples of primes in segment

### Option 2: Wheel factorization without arrays
- 6k±1 wheel: check only numbers ≡ ±1 mod 6
- Reduces checks by 2/3
- Still O(n√n) but 3x faster

### Option 3: On-the-fly sieve
- For each number n, check if divisible by any prime ≤ √n
- Store found primes in... can't store array
- But we can RECOMPUTE primes ≤ √n each time? No, too slow.

## BREAKTHROUGH IDEA: **Incremental prime list**

We can build list of primes as we find them, using integer encoding.

Example: Encode primes in base-2 representation across multiple u64?
No, need dynamic storage.

## REALITY CHECK: Zeta constraints
1. No dynamic arrays (except [dynamic]T which is slow)
2. No break in if statements (type checker bug)
3. Limited memory model

## SOLUTION PATH:
1. Fix type checker (allow break in if)
2. Implement segmented bit array with fixed-size computation
3. Achieve O(n log log n) performance