# PowerShell script to fix unsafe attributes in Rust 2024
$files = @(
    "src/std/collections/mod.rs",
    "src/std/io/mod.rs"
)

foreach ($file in $files) {
    $content = Get-Content $file -Raw
    # Replace #[no_mangle] with #[unsafe(no_mangle)]
    $content = $content -replace '\[no_mangle\]', '[unsafe(no_mangle)]'
    Set-Content $file $content
    Write-Host "Fixed $file"
}