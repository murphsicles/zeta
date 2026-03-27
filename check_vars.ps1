cd C:\Users\mummy\.openclaw\workspace\zeta-public

Write-Host "=== Checking how variables are handled in MIR generator ==="

# Look for variable handling
$content = Get-Content -Path src\middle\mir\gen.rs -Raw
if ($content -match "var_map") {
    Write-Host "Found var_map"
}

# Look for how Var nodes are handled in lower_expr
Write-Host "`n=== Searching for Var handling ==="
$lines = $content -split "`n"
for ($i = 0; $i -lt $lines.Count; $i++) {
    if ($lines[$i] -match "AstNode::Var") {
        Write-Host "Line $($i+1): $($lines[$i])"
        # Show a few lines after
        for ($j = 1; $j -le 5; $j++) {
            if ($i+$j -lt $lines.Count) {
                Write-Host "  $($lines[$i+$j])"
            }
        }
        Write-Host ""
    }
}