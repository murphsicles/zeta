// Benchmark runner for PrimeZeta submission
// Calls the Zeta implementation of Murphy's Sieve and runs for 5 seconds

use std::time::{Instant, Duration};

// External function from Zeta compiled code
extern "C" {
    fn murphy_sieve(limit: u64) -> u64;
}

fn main() {
    // Verify the implementation works
    println!("=== PrimeZeta Benchmark ===");
    println!("Testing Murphy's Sieve implementation...");
    
    // Test with known values
    let test_cases = vec![
        (3, 1),    // primes under 3: 2
        (10, 4),   // primes under 10: 2, 3, 5, 7
        (100, 25), // primes under 100: 25 primes
    ];
    
    let mut all_pass = true;
    for (limit, expected) in test_cases {
        unsafe {
            let result = murphy_sieve(limit);
            let pass = result == expected;
            println!("  limit={}: result={}, expected={}, {}", 
                     limit, result, expected, if pass { "✓" } else { "✗" });
            all_pass = all_pass && pass;
        }
    }
    
    if !all_pass {
        eprintln!("Tests failed!");
        std::process::exit(1);
    }
    
    println!("All tests passed!");
    
    // Run the benchmark for 5 seconds
    println!("\nRunning 5-second benchmark...");
    let start = Instant::now();
    let target_duration = Duration::from_secs(5);
    
    let mut iterations = 0;
    
    // Run benchmark loop
    while start.elapsed() < target_duration {
        unsafe {
            // Call the Zeta sieve function directly
            let _ = murphy_sieve(1_000_000);
        }
        iterations += 1;
    }
    
    let elapsed = start.elapsed();
    
    // Output in required format: label;iterations;total_time;num_threads;tags
    println!("\n=== Benchmark Results ===");
    println!("zeta;{};{:.3};1;algorithm=wheel;faithful=yes;bits=1", 
             iterations, elapsed.as_secs_f64());
    
    // Verify prime count for 1,000,000
    unsafe {
        let prime_count = murphy_sieve(1_000_000);
        println!("\nPrime count up to 1,000,000: {}", prime_count);
        println!("Expected: 78,498");
        println!("Match: {}", prime_count == 78498);
    }
}