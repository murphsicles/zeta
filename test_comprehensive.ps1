cd C:\Users\mummy\.openclaw\workspace\zeta-public

Write-Host "=== Testing comprehensive variable binding ==="
.\target\debug\zetac.exe test_variable_binding_comprehensive.z

Write-Host "`n=== Expected result: 180 (20 + 50 + 10 + 100) ==="