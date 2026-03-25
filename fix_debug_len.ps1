$files = @(
    "src/frontend/parser/expr.rs",
    "src/frontend/parser/top_level.rs"
)

foreach ($file in $files) {
    $content = Get-Content $file -Raw
    $content = $content -replace 'let debug_len = input\.chars\(\)\.take\(\d+\)\.map\(\|c\| c\.len_utf8\(\)\)\.sum\(\)', 'let debug_len: usize = input.chars().take($1).map(|c| c.len_utf8()).sum()'
    $content = $content -replace 'let debug_len = ', 'let debug_len: usize = '
    Set-Content $file $content
}