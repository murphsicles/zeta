#!/usr/bin/env python3
"""
Quick performance test for float parser optimization
Simulates the algorithmic improvement
"""

import time
import random

def simulate_old_float_parser(input_str):
    """Simulate character-by-character parsing (old method)"""
    chars = list(input_str)
    pos = 0
    has_digit = False
    
    # Parse integer part
    while pos < len(chars) and chars[pos].isdigit():
        has_digit = True
        pos += 1
    
    if not has_digit:
        return False
    
    # Check decimal point
    has_decimal = False
    if pos < len(chars) and chars[pos] == '.':
        has_decimal = True
        pos += 1
        
        # Parse fraction part
        has_fraction = False
        while pos < len(chars) and chars[pos].isdigit():
            has_fraction = True
            pos += 1
        
        if not has_fraction:
            return False
    
    return has_decimal

def simulate_new_float_parser(input_str):
    """Simulate byte slice parsing (new method)"""
    bytes_data = input_str.encode('utf-8')
    pos = 0
    
    # Parse integer part
    start_pos = pos
    while pos < len(bytes_data) and 48 <= bytes_data[pos] <= 57:  # '0'-'9'
        pos += 1
    
    if pos == start_pos:
        return False
    
    # Check decimal point
    has_decimal = False
    if pos < len(bytes_data) and bytes_data[pos] == 46:  # '.'
        has_decimal = True
        pos += 1
        
        # Parse fraction part
        fraction_start = pos
        while pos < len(bytes_data) and 48 <= bytes_data[pos] <= 57:
            pos += 1
        
        if pos == fraction_start:
            return False
    
    return has_decimal

def generate_test_cases(count=1000):
    """Generate random float strings for testing"""
    test_cases = []
    for _ in range(count):
        # Generate random float: digits.digits
        int_part = ''.join(str(random.randint(0, 9)) for _ in range(random.randint(1, 10)))
        frac_part = ''.join(str(random.randint(0, 9)) for _ in range(random.randint(1, 10)))
        test_cases.append(f"{int_part}.{frac_part}")
    return test_cases

def run_performance_test():
    """Run before/after performance comparison"""
    print("=== FLOAT PARSER OPTIMIZATION SIMULATION ===")
    print("")
    
    # Generate test cases
    test_cases = generate_test_cases(10000)
    print(f"Generated {len(test_cases)} test cases")
    
    # Test old parser
    print("\n1. Testing OLD parser (character iteration):")
    start = time.time()
    old_results = []
    for test in test_cases:
        old_results.append(simulate_old_float_parser(test))
    old_time = time.time() - start
    print(f"   Time: {old_time:.4f} seconds")
    print(f"   Rate: {len(test_cases)/old_time:.0f} parses/second")
    
    # Test new parser  
    print("\n2. Testing NEW parser (byte slice iteration):")
    start = time.time()
    new_results = []
    for test in test_cases:
        new_results.append(simulate_new_float_parser(test))
    new_time = time.time() - start
    print(f"   Time: {new_time:.4f} seconds")
    print(f"   Rate: {len(test_cases)/new_time:.0f} parses/second")
    
    # Verify results match
    if old_results == new_results:
        print("\n✅ Results match: Both parsers produce same output")
    else:
        print("\n❌ Results differ: Parsers produce different output")
        mismatches = sum(1 for o, n in zip(old_results, new_results) if o != n)
        print(f"   Mismatches: {mismatches}/{len(test_cases)}")
    
    # Calculate improvement
    improvement = (old_time - new_time) / old_time * 100
    speedup = old_time / new_time if new_time > 0 else 0
    
    print(f"\n📊 PERFORMANCE IMPROVEMENT:")
    print(f"   Time reduction: {improvement:.1f}%")
    print(f"   Speedup: {speedup:.2f}x")
    
    if speedup > 1:
        print(f"   ✅ OPTIMIZATION EFFECTIVE: {speedup:.2f}x faster")
    else:
        print(f"   ⚠️ No improvement or regression")
    
    # Algorithmic analysis
    print("\n🔍 ALGORITHMIC ANALYSIS:")
    print("   OLD: Character iteration with peekable() iterator")
    print("        - Iterator overhead per character")
    print("        - UTF-8 decoding for each character")
    print("        - Dynamic dispatch for peek()/next()")
    print("")
    print("   NEW: Byte slice with direct indexing")
    print("        - Direct memory access (O(1) indexing)")
    print("        - No iterator overhead")
    print("        - Simple integer comparisons")
    print("")
    print("   EXPECTED: 2-5x speedup for pure parsing logic")
    print("   ACTUAL SIMULATION: {:.2f}x speedup".format(speedup))

if __name__ == "__main__":
    run_performance_test()