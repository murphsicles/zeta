Write-Host "=== Checking Match Statement Implementation ==="
cd C:\Users\mummy\.openclaw\workspace\zeta-public

# Check AST definition
Write-Host "`n=== AST Definition (zeta_src/frontend/ast.z) ==="
if (Test-Path zeta_src\frontend\ast.z) {
    Select-String -Path zeta_src\frontend\ast.z -Pattern "match|Match" | Select-Object -First 10
}

# Check parser
Write-Host "`n=== Parser Implementation ==="
if (Test-Path src\frontend\parser.rs) {
    Select-String -Path src\frontend\parser.rs -Pattern "parse_match|Match" | Select-Object -First 10
}

# Check MIR generation
Write-Host "`n=== MIR Generation (src/middle/mir/gen.rs) ==="
if (Test-Path src\middle\mir\gen.rs) {
    Select-String -Path src\middle\mir\gen.rs -Pattern "match|Match" | Select-Object -First 15
}

# Check test files
Write-Host "`n=== Test Files ==="
dir test_match*.z