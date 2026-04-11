// Rust test of the Murphy's Sieve algorithm
fn sieve(limit: i64) -> i64 {
    if limit < 2 {
        return 0;
    }
    
    // Create sieve array for odd numbers only
    let sieve_size = (limit + 1) / 2;
    let mut sieve = vec![true; sieve_size as usize];
    
    // Mark 1 as not prime (index 0 = number 1)
    sieve[0] = false;
    
    let sqrt_limit = ((limit as f64).sqrt() as i64);
    
    // Process odd numbers only
    let mut i = 3;
    while i <= sqrt_limit {
        let idx = (i / 2) as usize;
        if sieve[idx] {
            // i is prime, mark its multiples
            let mut j = i * i;
            while j <= limit {
                sieve[(j / 2) as usize] = false;
                j += i * 2;
            }
        }
        i += 2;
    }
    
    // Count primes: start with 2 (the only even prime)
    let mut count = 1;
    
    // Count odd primes
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
}