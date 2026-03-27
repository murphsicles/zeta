cd C:\Users\mummy\.openclaw\workspace\zeta-public

Write-Host "=== Checking branches ==="
git branch -a

Write-Host "`n=== Trying to push with explicit refspec ==="
git push origin HEAD:v0.3.8