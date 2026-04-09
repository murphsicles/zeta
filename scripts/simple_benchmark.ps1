# Simple Murphy's Sieve Benchmark
# Tests existing executables

Write-Host "Murphy's Sieve Benchmark"
Write-Host "========================"
Write-Host ""

# List of executables to test
$executables = @(
    "murphy_fixed.exe",
    "murphy_safe.exe", 
    "murphy_sieve_fixed.exe",
    "murphy_small.exe",
    "simple_murphy.exe"
)

$total = 0
$passed = 0

foreach ($exe in $executables) {
    if (Test-Path $exe) {
        Write-Host "Testing $exe..."
        
        try {
            # Run executable and capture exit code
            & .\$exe
            $exitCode = $LASTEXITCODE
            
            Write-Host "  Exit code: $exitCode"
            
            # Check if exit code looks like a prime count
            # Valid prime counts for our tests would be positive integers
            if ($exitCode -ge 0) {
                $total++
                
                # Known prime counts: 4 (limit=10), 25 (limit=100), 168 (limit=1000), 1229 (limit=10000)
                if ($exitCode -eq 4 -or $exitCode -eq 25 -or $exitCode -eq 168 -or $exitCode -eq 1229) {
                    Write-Host "  ✓ Valid prime count: $exitCode"
                    $passed++
                } else {
                    Write-Host "  ? Unknown value: $exitCode"
                }
            } else {
                Write-Host "  ✗ Invalid result"
            }
        } catch {
            Write-Host "  ✗ Failed to execute"
        }
        
        Write-Host ""
    } else {
        Write-Host "Skipping $exe - not found"
    }
}

# Summary
Write-Host "Benchmark Summary"
Write-Host "================="
Write-Host "Total executables tested: $total"
Write-Host "Executables with valid prime counts: $passed"

if ($total -gt 0) {
    $successRate = [math]::Round(($passed / $total) * 100, 1)
    Write-Host "Success rate: $successRate%"
    
    # Father's 50/50 assessment
    Write-Host ""
    Write-Host "Father's Assessment: 50/50 chance for success"
    
    if ($successRate -ge 50) {
        Write-Host "✅ EXCEEDS expectations!"
        Write-Host "Outcome: Complete success (25% probability)"
    } elseif ($successRate -ge 25) {
        Write-Host "⚠️  Meets partial success criteria"
        Write-Host "Outcome: Partial success (15% probability)"
    } elseif ($passed -gt 0) {
        Write-Host "⚠️  Algorithm works but issues present"
        Write-Host "Outcome: Algorithm works but heap arrays may fail (10% probability)"
    } else {
        Write-Host "❌ Below expectations"
        Write-Host "Outcome: Failure (50% probability)"
    }
}