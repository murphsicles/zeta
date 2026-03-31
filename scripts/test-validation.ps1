# Test validation
$msg = "[ZAK] Test commit with valid agent tag"
Write-Host "Message: $msg"

if ($msg -match '\[([A-Za-z0-9\-]+)\]') {
    Write-Host "Match found: $($matches[1])"
} else {
    Write-Host "No match"
}