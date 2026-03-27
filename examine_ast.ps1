cd C:\Users\mummy\.openclaw\workspace\zeta-public

Write-Host "=== AST Variants with Var or Lit ==="
$content = Get-Content -Path zeta_src\frontend\ast.z -Raw
$lines = $content -split "`n"

foreach ($line in $lines) {
    if ($line -match "Var\(" -or $line -match "Lit\(" -or $line -match "Var\s*{" -or $line -match "Lit\s*{") {
        Write-Host $line
    }
}

Write-Host "`n=== Looking for Var variant definition ==="
for ($i = 0; $i -lt $lines.Count; $i++) {
    if ($lines[$i] -match "^\s*Var\s*\{") {
        Write-Host "Found at line $($i+1):"
        $j = $i
        while ($j -lt $lines.Count -and $lines[$j] -notmatch "^\s*\},?$") {
            Write-Host $lines[$j]
            $j++
        }
        if ($j -lt $lines.Count) {
            Write-Host $lines[$j]
        }
        break
    }
    if ($lines[$i] -match "^\s*Var\(") {
        Write-Host "Found at line $($i+1): $($lines[$i])"
        break
    }
}