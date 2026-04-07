#!/usr/bin/env python3
"""
Test the Murphy's Sieve algorithm logic.
This Python implementation mimics the Zeta code to verify correctness.
"""

def murphy_sieve(limit):
    """Python version of the Zeta Murphy's Sieve implementation."""
    if limit < 2:
        return 0
    if limit == 2:
        return 1
    
    # Create bit array: 1 = prime, 0 = composite
    bits = [1] * (limit + 1)
    
    # Mark 0 and 1 as composite
    bits[0] = 0
    bits[1] = 0
    
    # Wheel optimization: skip multiples of first 6 primes
    # Mark multiples of 2
    for i in range(4, limit + 1, 2):
        bits[i] = 0
    
    # Mark multiples of 3
    for i in range(6, limit + 1, 3):
        bits[i] = 0
    
    # Mark multiples of 5
    for i in range(10, limit + 1, 5):
        bits[i] = 0
    
    # Mark multiples of 7
    for i in range(14, limit + 1, 7):
        bits[i] = 0
    
    # Mark multiples of 11
    for i in range(22, limit + 1, 11):
        bits[i] = 0
    
    # Mark multiples of 13
    for i in range(26, limit + 1, 13):
        bits[i] = 0
    
    # Main sieve loop
    # Start from 17 (next prime after wheel primes)
    p = 17
    
    while p * p <= limit:
        if bits[p] == 1:
            # p is prime, mark its multiples
            multiple = p * p
            while multiple <= limit:
                bits[multiple] = 0
                multiple += p
        
        # Wheel increment: skip numbers divisible by wheel primes
        p += 1
        while p <= limit and (p % 2 == 0 or p % 3 == 0 or p % 5 == 0 or 
                             p % 7 == 0 or p % 11 == 0 or p % 13 == 0):
            p += 1
    
    # Count primes
    count = 0
    for i in range(2, limit + 1):
        if bits[i] == 1:
            count += 1
    
    return count

def test_algorithm():
    """Test the algorithm with known values."""
    test_cases = [
        (10, 4),      # primes under 10: 2,3,5,7
        (30, 10),     # primes under 30
        (100, 25),    # primes under 100
        (1000, 168),  # primes under 1000
    ]
    
    print("Testing Murphy's Sieve algorithm:")
    all_pass = True
    
    for limit, expected in test_cases:
        result = murphy_sieve(limit)
        status = "PASS" if result == expected else "FAIL"
        print(f"  limit={limit}: result={result}, expected={expected} {status}")
        if result != expected:
            all_pass = False
    
    # Final test: 1,000,000 should have 78,498 primes
    print("\nTesting with limit=1,000,000 (this may take a moment)...")
    result = murphy_sieve(1000000)
    expected = 78498
    status = "PASS" if result == expected else "FAIL"
    print(f"  result={result}, expected={expected} {status}")
    
    if result != expected:
        all_pass = False
    
    if all_pass:
        print("\nAll tests passed!")
        return 0
    else:
        print("\nSome tests failed!")
        return 1

if __name__ == "__main__":
    exit(test_algorithm())