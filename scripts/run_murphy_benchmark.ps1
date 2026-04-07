# Comprehensive Murphy's Sieve Benchmark Script
# Tests all existing implementations and reports results

Write-Host "=== Murphy's Sieve Enhanced Benchmark ==="
Write-Host "Testing with 50/50 chance assessment"
Write-Host ""

# Expected results for different limits
$expectedResults = @{
    "limit_10" = 4
    "limit_100" = 25
    "limit_1000" = 168
    "limit_10000" = 1229
}

# Test cases to run
$testCases = @(
    @{Name = "Small scale (limit=100)"; Expected = 25},
    @{Name = "Benchmark scale (limit=1000)"; Expected = 168},
    @{Name = "Medium scale (limit=10000)"; Expected = 1229}
)

# Executables to test
$executables = @(
    @{Name = "murphy_fixed.exe"; Path = ".\murphy_fixed.exe"},
    @{Name = "murphy_safe.exe"; Path = ".\murphy_safe.exe"},
    @{Name = "murphy_sieve_fixed.exe"; Path = ".\murphy_sieve_fixed.exe"},
    @{Name = "murphy_small.exe"; Path = ".\murphy_small.exe"},
    @{Name = "simple_murphy.exe"; Path = ".\simple_murphy.exe"}
)

# Results tracking
$results = @()
$totalTests = 0
$passedTests = 0

foreach ($exe in $executables) {
    if (Test-Path $exe.Path) {
        Write-Host "Testing $($exe.Name):"
        
        try {
            # Run the executable
            $output = & $exe.Path 2>$null
            $exitCode = $LASTEXITCODE
            
            # For now, we'll assume the exit code is the result
            # (This is how Zeta programs typically return values)
            $result = $exitCode
            
            # Check if result is reasonable (prime count should be positive integer)
            $isValid = $result -ge 0
            
            if ($isValid) {
                Write-Host "  Result: $result (exit code)"
                
                # Try to match against expected values
                $matched = $false
                foreach ($test in $testCases) {
                    if ($result -eq $test.Expected) {
                        Write-Host "  ✓ Matches expected: $($test.Name)"
                        $matched = $true
                        break
                    }
                }
                
                if (-not $matched) {
                    Write-Host "  ? Unknown result (not matching standard test cases)"
                }
                
                $results += @{
                    Executable = $exe.Name
                    Result = $result
                    Valid = $true
                    MatchedExpected = $matched
                }
                $totalTests++
                if ($matched) { $passedTests++ }
            } else {
                Write-Host "  ✗ Invalid result: $result"
                $results += @{
                    Executable = $exe.Name
                    Result = $result
                    Valid = $false
                    MatchedExpected = $false
                }
                $totalTests++
            }
        } catch {
            Write-Host "  ✗ Failed to execute: $_"
            $results += @{
                Executable = $exe.Name
                Result = "Execution failed"
                Valid = $false
                MatchedExpected = $false
            }
            $totalTests++
        }
        
        Write-Host ""
    } else {
        Write-Host "Skipping $($exe.Name) - not found"
    }
}

# Summary
Write-Host "=== Benchmark Summary ==="
Write-Host "Total tests run: $totalTests"
Write-Host "Passed tests (valid results): $passedTests"

if ($totalTests -gt 0) {
    $successRate = [math]::Round(($passedTests / $totalTests) * 100, 2)
    Write-Host "Success rate: $successRate%"
    
    # Assess against Father's 50/50 prediction
    if ($successRate -ge 50) {
        Write-Host "✅ EXCEEDS Father's 50/50 prediction!"
    } elseif ($successRate -ge 25) {
        Write-Host "⚠️  Meets partial success criteria (25%+)"
    } else {
        Write-Host "❌ Below Father's 50/50 prediction"
    }
}

# Detailed results
Write-Host "`n=== Detailed Results ==="
foreach ($result in $results) {
    $status = if ($result.Valid) { 
        if ($result.MatchedExpected) { "✅ PASS" } else { "⚠️  UNKNOWN" }
    } else { "❌ FAIL" }
    Write-Host "$status $($result.Executable): $($result.Result)"
}

# Final assessment based on enhanced testing criteria
Write-Host "`n=== Enhanced Testing Assessment ==="
Write-Host "Based on Father's 50/50 chance assessment:"

$assessment = @{
    "Complete success (all tests pass, good performance)" = 0.25
    "Partial success (correct but performance issues)" = 0.15
    "Algorithm works but heap arrays fail" = 0.10
    "Failure (crash, wrong values, infinite loop)" = 0.50
}

if ($passedTests -eq $totalTests -and $totalTests -gt 0) {
    Write-Host "Outcome: Complete success (25% probability)"
    Write-Host "✅ All executables produced valid results"
} elseif ($passedTests -gt 0) {
    Write-Host "Outcome: Partial success (15% probability)"
    Write-Host "⚠️  Some executables work, but not all"
} else {
    Write-Host "Outcome: Failure (50% probability)"
    Write-Host "❌ No valid results from executables"
}

Write-Host "`nNote: This benchmark tests existing compiled executables."
Write-Host "For full enhanced testing (heap arrays, performance timing," 
Write-Host "scale testing to 10000), compilation of new test cases is needed."