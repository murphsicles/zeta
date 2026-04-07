# Fix TupleLit to Tuple in proc_macro.rs
$content = Get-Content -Path "src/frontend/proc_macro.rs" -Raw
$content = $content -replace 'AstNode::TupleLit\(', 'AstNode::Tuple('
Set-Content -Path "src/frontend/proc_macro.rs" -Value $content
Write-Host "Fixed TupleLit to Tuple in proc_macro.rs"