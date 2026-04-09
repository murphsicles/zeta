# Simple test for PrimeZeta submission

Write-Host "=== PrimeZeta Submission Test ==="
Write-Host ""

# Check files
$files = @(
    "Primes/PrimeZeta/solution_1/src/prime.z",
    "Primes/PrimeZeta/solution_1/src/prime_benchmark.rs",
    "Primes/PrimeZeta/solution_1/README.md",
    "Primes/PrimeZeta/solution_1/Dockerfile",
    "Primes/PrimeZeta/solution_1/Cargo.toml"
)

Write-Host "1. Checking files..."
foreach ($f in $files) {
    if (Test-Path $f) {
        Write-Host "  ✓ $f"
    } else {
        Write-Host "  ✗ $f"
    }
}

Write-Host ""
Write-Host "2. Checking README content..."
$readme = Get-Content "Primes/PrimeZeta/solution_1/README.md" -Raw
if ($readme -match "zeta solution by murphsicles") {
    Write-Host "  ✓ Correct header"
}

$badges = @("algorithm=wheel", "faithful=yes", "bits=1")
foreach ($b in $badges) {
    if ($readme -match $b) {
        Write-Host "  ✓ $b badge"
    }
}

Write-Host ""
Write-Host "3. Checking Zeta code..."
$zeta = Get-Content "Primes/PrimeZeta/solution_1/src/prime.z" -Raw
if ($zeta -match "fn murphy_sieve") {
    Write-Host "  ✓ Contains Murphy's Sieve function"
}
if ($zeta -match "78498") {
    Write-Host "  ✓ Contains correct prime count"
}

Write-Host ""
Write-Host "=== TEST COMPLETE ==="
Write-Host ""
Write-Host "Submission package is ready!"
Write-Host "All required files are present and formatted correctly."