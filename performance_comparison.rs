// PERFORMANCE COMPARISON: Old vs New PrimeZeta implementations
// Investigate why optimized version is slower than 2-day-old version

use std::time::Instant;

// ==================== OLD IMPLEMENTATION (2 days ago - 1.43x faster than C) ====================
// This is the simple wheel sieve from prime.z (April 5th)
fn old_primezeta_simple(limit: u64) -> u64 {
    if limit < 2 {
        return 0;
    }
    if limit == 2 {
        return 1;
    }
    
    // Simple bool array (like the original)
    let array_size = (limit + 1) as usize;
    let mut bits = vec![1u8; array_size];
    
    bits[0] = 0;
    bits[1] = 0;
    
    // Simple marking of multiples (no wheel optimization in this test)
    let sqrt_limit = (limit as f64).sqrt() as u64;
    
    for i in 2..=sqrt_limit {
        if bits[i as usize] == 1 {
            let mut j = i * i;
            while j <= limit {
                bits[j as usize] = 0;
                j += i;
            }
        }
    }
    
    // Count primes
    let mut count = 0;
    for i in 2..=limit {
        if bits[i as usize] == 1 {
            count += 1;
        }
    }
    
    count
}

// ==================== NEW IMPLEMENTATION (current - 93% of C) ====================
fn new_primezeta_optimized(limit: usize) -> usize {
    if limit <= 1 {
        return 0;
    }
    
    let word_size = (limit + 63) / 64;
    let mut sieve = vec![u64::MAX; word_size];
    
    // Clear bits for 0 and 1
    sieve[0] &= !(1 << 0);
    if limit >= 1 {
        sieve[0] &= !(1 << 1);
    }
    
    let sqrt_limit = (limit as f64).sqrt() as usize;
    for i in 2..=sqrt_limit {
        let word_idx = i / 64;
        let bit_mask = 1 << (i % 64);
        if (sieve[word_idx] & bit_mask) != 0 {
            let mut j = i * i;
            while j < limit {
                let j_word_idx = j / 64;
                let j_bit_mask = 1 << (j % 64);
                sieve[j_word_idx] &= !j_bit_mask;
                j += i;
            }
        }
    }
    
    // Count primes
    let mut count = 0;
    for i in 0..limit {
        let word_idx = i / 64;
        let bit_mask = 1 << (i % 64);
        if (sieve[word_idx] & bit_mask) != 0 {
            count += 1;
        }
    }
    
    count
}

// ==================== BENCHMARK COMPARISON ====================
fn benchmark_comparison() {
    println!("========================================================");
    println!("PERFORMANCE REGRESSION INVESTIGATION");
    println!("========================================================");
    println!("Comparing 2-day-old implementation vs current optimized version");
    println!("");
    
    let test_limits = [1000, 10000, 50000, 100000, 1_000_000];
    let expected_counts = [168, 1229, 5133, 9592, 78498];
    
    println!("=== CORRECTNESS VERIFICATION ===");
    let mut old_correct = true;
    let mut new_correct = true;
    
    for i in 0..test_limits.len() {
        let limit = test_limits[i];
        let expected = expected_counts[i];
        
        // Test old implementation
        let old_start = Instant::now();
        let old_result = old_primezeta_simple(limit as u64);
        let old_duration = old_start.elapsed();
        let old_ok = old_result as usize == expected;
        old_correct = old_correct && old_ok;
        
        // Test new implementation  
        let new_start = Instant::now();
        let new_result = new_primezeta_optimized(limit);
        let new_duration = new_start.elapsed();
        let new_ok = new_result == expected;
        new_correct = new_correct && new_ok;
        
        println!("limit={}:", limit);
        println!("  Old: {} primes in {:?} - {}", 
                old_result, old_duration, if old_ok { "✅" } else { "❌" });
        println!("  New: {} primes in {:?} - {}", 
                new_result, new_duration, if new_ok { "✅" } else { "❌" });
        
        let speed_ratio = old_duration.as_nanos() as f64 / new_duration.as_nanos() as f64;
        println!("  Speed ratio (old/new): {:.2}x - {}", 
                speed_ratio, 
                if speed_ratio > 1.0 { "Old is FASTER" } else { "New is FASTER" });
        println!();
    }
    
    println!("=== PERFORMANCE ANALYSIS ===");
    
    // Run detailed benchmark at competition scale
    let competition_limit = 1_000_000;
    println!("\nCompetition-scale benchmark (limit={}):", competition_limit);
    
    // Warm up
    let _ = old_primezeta_simple(1000);
    let _ = new_primezeta_optimized(1000);
    
    // Benchmark old
    let old_start = Instant::now();
    let old_result = old_primezeta_simple(competition_limit as u64);
    let old_time = old_start.elapsed();
    
    // Benchmark new
    let new_start = Instant::now();
    let new_result = new_primezeta_optimized(competition_limit);
    let new_time = new_start.elapsed();
    
    println!("Old implementation: {} primes in {:?}", old_result, old_time);
    println!("New implementation: {} primes in {:?}", new_result, new_time);
    
    let performance_ratio = new_time.as_nanos() as f64 / old_time.as_nanos() as f64;
    println!("Performance ratio (new/old): {:.2}x", performance_ratio);
    
    if performance_ratio > 1.0 {
        println!("❌ REGRESSION DETECTED: New implementation is {:.2}x SLOWER", performance_ratio);
    } else {
        println!("✅ IMPROVEMENT: New implementation is {:.2}x FASTER", 1.0 / performance_ratio);
    }
    
    println!("\n=== MEMORY USAGE COMPARISON ===");
    println!("Old implementation (bool array):");
    println!("  - Memory: {} bytes ({} KB)", 
            competition_limit + 1, 
            (competition_limit + 1) / 1024);
    
    println!("New implementation (u64 bit array):");
    let word_size = (competition_limit + 63) / 64;
    let memory_bytes = word_size * 8;
    println!("  - Memory: {} bytes ({} KB)", 
            memory_bytes, 
            memory_bytes / 1024);
    
    let memory_ratio = (competition_limit + 1) as f64 / memory_bytes as f64;
    println!("Memory improvement: {:.1}x reduction", memory_ratio);
    
    println!("\n=== ROOT CAUSE ANALYSIS ===");
    println!("Possible reasons for performance regression:");
    println!("1. Bit manipulation overhead: Bit operations are slower than byte operations");
    println!("2. Cache effects: u64 array may have worse cache locality than bool array");
    println!("3. Loop overhead: More complex indexing calculations");
    println!("4. Memory allocation: vec![] vs direct array (in old Zeta implementation)");
    println!("5. Algorithm differences: Old had wheel optimization, new is simple sieve");
    
    println!("\n=== RECOMMENDATION ===");
    if performance_ratio > 1.2 {
        println!("❌ CRITICAL: Performance regression too large (>20%)");
        println!("   Consider: Revert to old algorithm or hybrid approach");
    } else if performance_ratio > 1.05 {
        println!("⚠️  SIGNIFICANT: Performance regression detected (5-20%)");
        println!("   Consider: Optimize bit operations or improve cache locality");
    } else {
        println!("✅ ACCEPTABLE: Minor performance difference");
        println!("   Memory improvement may justify small performance cost");
    }
}

fn main() {
    benchmark_comparison();
}