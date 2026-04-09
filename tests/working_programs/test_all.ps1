# Test script to verify all working programs
Write-Host "=== ZETA COMPILER VERIFICATION TEST ==="
Write-Host "Compiler: v0.3.22"
Write-Host "Date: $(Get-Date)"
Write-Host ""

$testFiles = @(
    @{Name="01_hello_world.z"; Expected=42},
    @{Name="02_arithmetic.z"; Expected=72},
    @{Name="03_variables.z"; Expected=300},
    @{Name="04_function_calls.z"; Expected=60},
    @{Name="05_type_annotations.z"; Expected=300}
)

$allPassed = $true

foreach ($test in $testFiles) {
    Write-Host "Testing $($test.Name)..." -NoNewline
    
    $output = cargo run --bin zetac -- "tests/working_programs/$($test.Name)" 2>&1
    $resultLine = $output | Select-String -Pattern "Result: (\d+)"
    
    if ($resultLine) {
        $actual = [int]$resultLine.Matches.Groups[1].Value
        if ($actual -eq $test.Expected) {
            Write-Host " ✅ PASS (Expected: $($test.Expected), Got: $actual)" -ForegroundColor Green
        } else {
            Write-Host " ❌ FAIL (Expected: $($test.Expected), Got: $actual)" -ForegroundColor Red
            $allPassed = $false
        }
    } else {
        Write-Host " ❌ FAIL (No result output)" -ForegroundColor Red
        $allPassed = $false
    }
}

Write-Host ""
if ($allPassed) {
    Write-Host "=== ALL TESTS PASSED ===" -ForegroundColor Green
} else {
    Write-Host "=== SOME TESTS FAILED ===" -ForegroundColor Red
}