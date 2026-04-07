Write-Host "Testing actual compilation of Zeta files..."
Write-Host "=========================================="

# First, let me check if there's a way to compile Zeta code
# Looking at the main.z file, it seems to have a compile_and_run_zeta function
# Let me try to run the zetac binary with a simple test

$test_code = @"
fn main() -> i64 {
    return 42;
}
"@

Write-Host "Testing simple Zeta code compilation..."
$test_code | Out-File -FilePath "test_simple.z" -Encoding UTF8

# Try to run the zetac binary
Write-Host "Attempting to compile and run test_simple.z..."
$output = cargo run --bin zetac test_simple.z 2>&1
Write-Host "Output:"
$output | ForEach-Object { Write-Host $_ }

# Clean up
Remove-Item -Path "test_simple.z" -ErrorAction SilentlyContinue