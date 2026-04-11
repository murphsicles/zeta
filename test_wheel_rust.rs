// Rust test of the Murphy's Sieve with wheel optimization
fn sieve(limit: i64) -> i64 {
    if limit < 2 {
        return 0;
    }
    
    if limit == 2 {
        return 1;
    }
    
    // Bit array for odd numbers only
    let sieve_size = (limit + 1) / 2;
    let mut sieve = vec![true; sieve_size as usize];
    
    // Mark 1 as not prime
    sieve[0] = false;
    
    let sqrt_limit = (limit as f64).sqrt() as i64;
    
    // Process prime 3
    let mut j = 9;
    while j <= limit {
        sieve[(j / 2) as usize] = false;
        j += 6;
    }
    
    // 30030-wheel pattern
    let mut i = 5;
    while i <= sqrt_limit {
        if sieve[(i / 2) as usize] {
            let step = i * 2;
            let mut j = i * i;
            while j <= limit {
                sieve[(j / 2) as usize] = false;
                j += step;
            }
        }
        
        i += 2;
        
        if i <= sqrt_limit {
            if sieve[(i / 2) as usize] {
                let step = i * 2;
                let mut j = i * i;
                while j <= limit {
                    sieve[(j / 2) as usize] = false;
                    j += step;
                }
            }
            
            i += 4;
        }
    }
    
    // Count primes
    let mut count = 1;  // Count 2
    
    let mut i = 1;
    while (i as usize) < sieve.len() {
        if sieve[i as usize] {
            count += 1;
        }
        i += 1;
    }
    
    count
}

fn main() {
    let result = sieve(1_000_000);
    println!("Result: {}", result);
    println!("Expected: 78498");
    println!("Match: {}", result == 78498);
    
    // Test some smaller limits
    println!("\nTesting smaller limits:");
    let test_cases = [(10, 4), (100, 25), (1000, 168), (10000, 1229)];
    for &(limit, expected) in &test_cases {
        let result = sieve(limit);
        println!("limit={}: {} (expected {}) {}", limit, result, expected, if result == expected { "✓" } else { "✗" });
    }
}