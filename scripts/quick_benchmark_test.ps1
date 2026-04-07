#!/usr/bin/env pwsh
# Quick benchmark test

Write-Host "Testing PrimeZeta naive algorithm..."

# Create a simple Rust test
$rustCode = @"
fn main() {
    let limit = 1000;
    let mut count = 0;
    
    let start = std::time::Instant::now();
    
    for n in 2..=limit {
        let mut is_prime = true;
        for i in 2..n {
            if n % i == 0 {
                is_prime = false;
                break;
            }
        }
        if is_prime {
            count += 1;
        }
    }
    
    let duration = start.elapsed();
    println!("Naive limit={}: {} primes, Time: {:?}", limit, count, duration);
    println!("time_ms: {:.4}", duration.as_secs_f64() * 1000.0);
}
"@

$rustCode | Out-File -FilePath "target\quick_test.rs" -Encoding UTF8

# Compile and run
rustc "target\quick_test.rs" -O -o "target\quick_test.exe"
if (Test-Path "target\quick_test.exe") {
    $stopwatch = [System.Diagnostics.Stopwatch]::StartNew()
    $output = & ".\target\quick_test.exe"
    $stopwatch.Stop()
    
    Write-Host "Output: $output"
    Write-Host "Total time: $($stopwatch.Elapsed.TotalMilliseconds)ms"
} else {
    Write-Host "Compilation failed"
}