cd C:\Users\mummy\.openclaw\workspace\zeta-public

Write-Host "=== Checking git status ==="
git status

Write-Host "`n=== Checking what files were modified ==="
git diff --name-only

Write-Host "`n=== Checking diff for gen.rs ==="
git diff src/middle/mir/gen.rs | head -50

Write-Host "`n=== Adding test files ==="
git add test_variable_binding.z test_variable_binding_comprehensive.z

Write-Host "`n=== Adding modified source file ==="
git add src/middle/mir/gen.rs

Write-Host "`n=== Committing changes ==="
git commit -m "[v0.3.9] Implement variable binding in match patterns

- Add support for variable binding patterns in match arms
- Variables in patterns now bind to scrutinee value
- Works with shadowing and nested matches
- Comprehensive tests added
- Part of Pattern Matching Enhancements feature"

Write-Host "`n=== Pushing to GitHub ==="
git push origin v0.3.8