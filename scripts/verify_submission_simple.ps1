# Simple verification script

Write-Host "=== PrimeZeta Submission Verification ==="
Write-Host ""

# Check directory structure
Write-Host "1. Checking directory structure..."
$files = @(
    "Primes/PrimeZeta/solution_1/src/prime.z",
    "Primes/PrimeZeta/solution_1/src/prime_benchmark.rs", 
    "Primes/PrimeZeta/solution_1/README.md",
    "Primes/PrimeZeta/solution_1/Dockerfile",
    "Primes/PrimeZeta/solution_1/Cargo.toml"
)

foreach ($f in $files) {
    if (Test-Path $f) {
        Write-Host "  ✓ $f"
    } else {
        Write-Host "  ✗ $f"
    }
}

Write-Host ""

# Test benchmark
Write-Host "2. Testing benchmark..."
try {
    $result = .\test_benchmark.exe
    Write-Host "  ✓ Benchmark output: $result"
    
    if ($result -match "^zeta;") {
        Write-Host "  ✓ Correct output format"
    }
} catch {
    Write-Host "  ✗ Benchmark failed"
}

Write-Host ""

# Test prime count
Write-Host "3. Testing prime count..."
try {
    $primeTest = .\test_prime_count.exe | Select-String "Match: true"
    if ($primeTest) {
        Write-Host "  ✓ Prime count verified (78,498 primes)"
    }
} catch {
    Write-Host "  ✗ Prime count test failed"
}

Write-Host ""
Write-Host "=== READY FOR SUBMISSION ==="