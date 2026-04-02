# Fix all #[no_mangle] attributes in the project
$rustFiles = Get-ChildItem -Path . -Recurse -Filter "*.rs" | Where-Object { $_.FullName -match "src\\std" }

foreach ($file in $rustFiles) {
    $content = Get-Content $file.FullName -Raw
    $originalContent = $content
    $content = $content -replace '\[no_mangle\]', '[unsafe(no_mangle)]'
    
    if ($content -ne $originalContent) {
        Set-Content $file.FullName $content
        Write-Host "Fixed $($file.FullName)"
    }
}