# Fix expr.rs
$exprContent = Get-Content "src/frontend/parser/expr.rs" -Raw
$exprContent = $exprContent -replace 'take\(\$1\)', 'take(20)'
$exprContent = $exprContent -replace 'take\(20\).*?input first 50 chars', 'take(50).map(|c| c.len_utf8()).sum();$n    println!("[PARSER DEBUG] parse_expr called, input first 50 chars"'
Set-Content "src/frontend/parser/expr.rs" $exprContent

# Fix top_level.rs
$topLevelContent = Get-Content "src/frontend/parser/top_level.rs" -Raw
$topLevelContent = $topLevelContent -replace 'take\(\$1\)', 'take(50)'
$topLevelContent = $topLevelContent -replace 'take\(\$1\)', 'take(100)'
Set-Content "src/frontend/parser/top_level.rs" $topLevelContent