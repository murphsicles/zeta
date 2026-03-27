# TODO Discovery Script for Windows PowerShell

Write-Host "# TODO Scan - $(Get-Date -Format 'yyyy-MM-dd HH:mm')" -ForegroundColor Red
Write-Host ""

Write-Host "## High Priority (TODO/FIXME)" -ForegroundColor Yellow
Write-Host ""

# Find TODOs and FIXMEs
Get-ChildItem -Path "src", "tests" -Include "*.rs", "*.z" -Recurse | ForEach-Object {
    $content = Get-Content $_.FullName -Raw
    $lines = $content -split "`n"
    
    for ($i = 0; $i -lt $lines.Count; $i++) {
        if ($lines[$i] -match "(TODO|FIXME)") {
            $lineNum = $i + 1
            $match = $matches[1]
            $desc = $lines[$i].Trim()
            Write-Host "### $($_.Name):$lineNum - $match" -ForegroundColor Cyan
            Write-Host "  $desc" -ForegroundColor Gray
            Write-Host ""
        }
    }
}

Write-Host "## Placeholder Logic" -ForegroundColor Yellow
Write-Host ""

# Find placeholder patterns
$placeholderPatterns = @("placeholder", "stub", "hack", "XXX", "magic", "temp", "dummy")
Get-ChildItem -Path "src", "tests" -Include "*.rs", "*.z" -Recurse | ForEach-Object {
    $content = Get-Content $_.FullName -Raw
    $lines = $content -split "`n"
    
    for ($i = 0; $i -lt $lines.Count; $i++) {
        foreach ($pattern in $placeholderPatterns) {
            if ($lines[$i] -match $pattern) {
                $lineNum = $i + 1
                $desc = $lines[$i].Trim()
                Write-Host "### $($_.Name):$lineNum - Placeholder ($pattern)" -ForegroundColor Magenta
                Write-Host "  $desc" -ForegroundColor Gray
                Write-Host ""
                break
            }
        }
    }
}

Write-Host "## Magic Numbers/Values" -ForegroundColor Yellow
Write-Host ""

# Find magic numbers (simple detection)
$magicValues = @(" 0,", " 1,", " 2,", " 3,", " 4,", " 5,", " true", " false", " null", " nil")
Get-ChildItem -Path "src" -Include "*.rs", "*.z" -Recurse | ForEach-Object {
    $content = Get-Content $_.FullName -Raw
    $lines = $content -split "`n"
    
    for ($i = 0; $i -lt $lines.Count; $i++) {
        foreach ($magic in $magicValues) {
            if ($lines[$i] -match "$magic(?!\w)" -and $lines[$i] -notmatch "test" -and $lines[$i] -notmatch "0\.0") {
                $lineNum = $i + 1
                $desc = $lines[$i].Trim()
                Write-Host "### $($_.Name):$lineNum - Magic value" -ForegroundColor Green
                Write-Host "  $desc" -ForegroundColor Gray
                Write-Host ""
                break
            }
        }
    }
}

Write-Host "## Summary" -ForegroundColor Red
Write-Host "Run this script daily to track TODOs and placeholders." -ForegroundColor Gray
Write-Host "Add found items to TODO_TRACKING.md with owner and due date." -ForegroundColor Gray