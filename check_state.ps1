Write-Host "=== Checking Zeta Bootstrap Status ==="
Write-Host "Current time: $(Get-Date -Format 'yyyy-MM-dd HH:mm') GMT"
Write-Host ""

# Check workspace git status
Write-Host "Workspace Git Status:"
cd C:\Users\mummy\.openclaw\workspace
git status
Write-Host ""

# Check last commit
Write-Host "Last 3 commits:"
git log --oneline -3
Write-Host ""

# Check zeta-public directory
Write-Host "Zeta-public directory contents:"
cd C:\Users\mummy\.openclaw\workspace\zeta-public
dir | Select-Object -First 10
Write-Host ""

# Check if there's a git repo in zeta-public
Write-Host "Checking for git in zeta-public:"
if (Test-Path .git) {
    Write-Host "Found .git directory in zeta-public"
    git status
} else {
    Write-Host "No .git directory in zeta-public - it's part of workspace repo"
}