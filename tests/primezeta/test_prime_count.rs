// Test prime count verification

fn wheel_sieve(limit: u64) -> Vec<u64> {
    if limit < 2 {
        return Vec::new();
    }
    
    if limit == 2 {
        return vec![2];
    }
    
    let sieve_size = ((limit - 1) / 2) as usize;
    let mut sieve = vec![true; sieve_size];
    
    let sqrt_limit = (limit as f64).sqrt() as u64;
    
    for i in 0..sieve_size {
        let p = 2 * i as u64 + 3;
        if p > sqrt_limit {
            break;
        }
        
        if sieve[i] {
            let mut j = (p * p - 3) / 2;
            while j < sieve_size as u64 {
                sieve[j as usize] = false;
                j += p;
            }
        }
    }
    
    let mut primes = Vec::with_capacity(sieve_size / 2);
    primes.push(2);
    
    for i in 0..sieve_size {
        if sieve[i] {
            primes.push(2 * i as u64 + 3);
        }
    }
    
    primes
}

fn main() {
    let primes = wheel_sieve(1_000_000);
    println!("Prime count up to 1,000,000: {}", primes.len());
    println!("Expected: 78498");
    println!("Match: {}", primes.len() == 78498);
    
    // Show first 20 primes
    println!("\nFirst 20 primes:");
    for i in 0..20.min(primes.len()) {
        print!("{} ", primes[i]);
    }
    println!();
    
    // Show last 20 primes
    println!("\nLast 20 primes:");
    let start = primes.len().saturating_sub(20);
    for i in start..primes.len() {
        print!("{} ", primes[i]);
    }
    println!();
}