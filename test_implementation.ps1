cd C:\Users\mummy\.openclaw\workspace\zeta-public

Write-Host "=== Building Zeta compiler ==="
cargo build

if ($LASTEXITCODE -eq 0) {
    Write-Host "`n=== Build successful! Testing variable binding ==="
    
    # Test the variable binding pattern
    Write-Host "`n=== Testing test_variable_binding.z ==="
    .\target\debug\zetac.exe test_variable_binding.z
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "`n=== Running compiled program ==="
        .\test_variable_binding.exe
        Write-Host "Exit code: $LASTEXITCODE (should be 43)"
    } else {
        Write-Host "Compilation failed with exit code: $LASTEXITCODE"
    }
} else {
    Write-Host "Build failed with exit code: $LASTEXITCODE"
}