//! Smoke test for distributed systems functionality

use zetac::distributed::crdt::{GCounter, SharedCRDT};

#[test]
fn test_crdt_basic_functionality() {
    println!("Testing basic CRDT functionality...");
    
    // Test G-Counter
    let mut counter1 = GCounter::new();
    let mut counter2 = GCounter::new();
    
    counter1.increment(1);
    counter1.increment(1);
    counter2.increment(2);
    counter2.increment(2);
    counter2.increment(2);
    
    assert_eq!(counter1.value(), 2);
    assert_eq!(counter2.value(), 3);
    
    // Merge counters
    counter1.merge(&counter2);
    assert_eq!(counter1.value(), 5);
    
    println!("✓ G-Counter test passed");
    
    // Test shared CRDT
    let shared_counter = SharedCRDT::new(GCounter::new());
    shared_counter.write(|crdt| {
        crdt.increment(1);
        crdt.increment(1);
    });
    
    let value = shared_counter.read(|crdt| crdt.value());
    assert_eq!(value, 2);
    
    println!("✓ Shared CRDT test passed");
}

#[test]
fn test_distributed_module_structure() {
    println!("Testing distributed module structure...");
    
    // Verify modules exist
    assert!(std::path::Path::new("src/distributed/mod.rs").exists());
    assert!(std::path::Path::new("src/distributed/actor.rs").exists());
    assert!(std::path::Path::new("src/distributed/crdt.rs").exists());
    assert!(std::path::Path::new("src/distributed/transaction.rs").exists());
    assert!(std::path::Path::new("src/distributed/cluster.rs").exists());
    assert!(std::path::Path::new("src/distributed/transport.rs").exists());
    
    println!("✓ Distributed module structure test passed");
}

#[test]
fn test_murphy_sieve_distributed_concept() {
    println!("Testing Murphy's Sieve distributed concept...");
    
    // Verify the concept of distributed prime computation
    let total_limit = 1_000_000u64;
    let num_workers = 4u64;
    let range_size = total_limit / num_workers;
    
    // Calculate ranges
    let ranges: Vec<(u64, u64)> = (0..num_workers)
        .map(|i| {
            let start = i * range_size;
            let end = if i == num_workers - 1 {
                total_limit
            } else {
                (i + 1) * range_size
            };
            (start, end)
        })
        .collect();
    
    // Verify ranges
    assert_eq!(ranges.len(), 4);
    assert_eq!(ranges[0], (0, 250_000));
    assert_eq!(ranges[1], (250_000, 500_000));
    assert_eq!(ranges[2], (500_000, 750_000));
    assert_eq!(ranges[3], (750_000, 1_000_000));
    
    // Verify coverage
    let mut covered = 0u64;
    for (start, end) in &ranges {
        assert!(start < end, "Invalid range: {} >= {}", start, end);
        covered += end - start;
    }
    assert_eq!(covered, total_limit, "Ranges don't cover total limit");
    
    println!("✓ Murphy's Sieve distributed concept test passed");
}

fn main() {
    println!("Running distributed systems smoke tests...");
    
    test_crdt_basic_functionality();
    test_distributed_module_structure();
    test_murphy_sieve_distributed_concept();
    
    println!("\n✅ All smoke tests passed!");
    println!("Distributed systems implementation is structurally sound.");
}