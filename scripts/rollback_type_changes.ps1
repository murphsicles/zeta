# Rollback script in case type system integration fails
# This reverts MIR type changes to restore compilation

Write-Host "=== ROLLBACK TYPE SYSTEM CHANGES ===" -ForegroundColor Yellow

# 1. Revert MIR type changes
Write-Host "1. Reverting MIR type changes..." -ForegroundColor Yellow

# Revert mir.rs changes
$mirContent = Get-Content -Path "src/middle/mir/mir.rs" -Raw
$mirContent = $mirContent -replace 'use crate::middle::types::Type;', ''
$mirContent = $mirContent -replace 'pub type_map: HashMap<u32, Type>,', 'pub type_map: HashMap<u32, String>,'
$mirContent = $mirContent -replace 'type_args: Vec<Type>,', 'type_args: Vec<String>,'
Set-Content -Path "src/middle/mir/mir.rs" -Value $mirContent

# Revert mir/gen.rs changes  
$mirGenContent = Get-Content -Path "src/middle/mir/gen.rs" -Raw
$mirGenContent = $mirGenContent -replace 'use crate::middle::types::Type;', ''
$mirGenContent = $mirGenContent -replace 'type_map: HashMap<u32, Type>,', 'type_map: HashMap<u32, String>,'
$mirGenContent = $mirGenContent -replace 'Type::I64', '"i64".to_string()'
$mirGenContent = $mirGenContent -replace 'Type::I32', '"i32".to_string()'
$mirGenContent = $mirGenContent -replace 'Type::Bool', '"bool".to_string()'
$mirGenContent = $mirGenContent -replace 'Type::Str', '"str".to_string()'
Set-Content -Path "src/middle/mir/gen.rs" -Value $mirGenContent

Write-Host "✅ MIR changes reverted" -ForegroundColor Green

# 2. Test compilation
Write-Host "2. Testing compilation..." -ForegroundColor Yellow
cargo check
if ($LASTEXITCODE -ne 0) {
    Write-Host "❌ Compilation still broken" -ForegroundColor Red
    Write-Host "Manual intervention required" -ForegroundColor Red
    exit 1
}

Write-Host "✅ Compilation restored" -ForegroundColor Green
Write-Host "`n=== ROLLBACK COMPLETE ===" -ForegroundColor Yellow
Write-Host "Type system changes reverted to working state" -ForegroundColor Green