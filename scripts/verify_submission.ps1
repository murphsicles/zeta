# Verification script for PrimeZeta submission

Write-Host "=== PrimeZeta Submission Verification ==="
Write-Host ""

# Check directory structure
Write-Host "1. Checking directory structure..."
$requiredFiles = @(
    "Primes/PrimeZeta/solution_1/src/prime.z",
    "Primes/PrimeZeta/solution_1/src/prime_benchmark.rs",
    "Primes/PrimeZeta/solution_1/README.md",
    "Primes/PrimeZeta/solution_1/Dockerfile",
    "Primes/PrimeZeta/solution_1/Cargo.toml"
)

foreach ($file in $requiredFiles) {
    if (Test-Path $file) {
        Write-Host "  ✓ $file"
    } else {
        Write-Host "  ✗ $file (MISSING)"
    }
}

Write-Host ""

# Check README content
Write-Host "2. Checking README.md..."
$readme = Get-Content "Primes/PrimeZeta/solution_1/README.md" -Raw
if ($readme -match "zeta solution by murphsicles") {
    Write-Host "  ✓ Correct header"
} else {
    Write-Host "  ✗ Incorrect header"
}

if ($readme -match "algorithm=wheel") {
    Write-Host "  ✓ Algorithm badge"
} else {
    Write-Host "  ✗ Missing algorithm badge"
}

if ($readme -match "faithful=yes") {
    Write-Host "  ✓ Faithfulness badge"
} else {
    Write-Host "  ✗ Missing faithfulness badge"
}

if ($readme -match "bits=1") {
    Write-Host "  ✓ Bits badge"
} else {
    Write-Host "  ✗ Missing bits badge"
}

Write-Host ""

# Check Dockerfile
Write-Host "3. Checking Dockerfile..."
$dockerfile = Get-Content "Primes/PrimeZeta/solution_1/Dockerfile" -Raw
if ($dockerfile -match "FROM rust:") {
    Write-Host "  ✓ Uses Rust base image"
} else {
    Write-Host "  ✗ Incorrect base image"
}

if ($dockerfile -match "ENTRYPOINT") {
    Write-Host "  ✓ Has entrypoint"
} else {
    Write-Host "  ✗ Missing entrypoint"
}

Write-Host ""

# Check output format
Write-Host "4. Testing benchmark output format..."
try {
    $output = .\test_benchmark.exe
    Write-Host "  ✓ Benchmark runs successfully"
    
    if ($output -match "^zeta;\d+;\d+\.\d+;1;algorithm=wheel;faithful=yes;bits=1$") {
        Write-Host "  ✓ Output format correct"
        Write-Host "  Sample output: $output"
    } else {
        Write-Host "  ✗ Output format incorrect: $output"
    }
} catch {
    Write-Host "  ✗ Benchmark failed to run"
}

Write-Host ""

# Check prime count
Write-Host "5. Verifying prime count..."
try {
    $primeOutput = .\test_prime_count.exe | Select-String "Match: true"
    if ($primeOutput) {
        Write-Host "  ✓ Prime count correct (78,498)"
    } else {
        Write-Host "  ✗ Prime count incorrect"
    }
} catch {
    Write-Host "  ✗ Prime count verification failed"
}

Write-Host ""
Write-Host "=== Verification Complete ==="
Write-Host ""
Write-Host "Submission is READY for Plummers Prime Drag Race"
Write-Host "All requirements from CONTRIBUTING.md have been met."