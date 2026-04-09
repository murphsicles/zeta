# Test Zeta compiler in Docker (Linux environment)
Write-Host "=== Testing Zeta Compiler in Docker ===" -ForegroundColor Cyan
Write-Host ""

# Create a simple test container
$dockerCommand = @'
echo "Installing dependencies..."
apt-get update && apt-get install -y curl build-essential 2>/dev/null

echo "Installing Rust..."
curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
export PATH="/root/.cargo/bin:$PATH"

echo "Building Zeta compiler..."
cargo build --release 2>&1 | tail -5

echo ""
echo "=== Testing While Loop Fix ==="
if [ -f target/release/zetac ]; then
    echo "Running test_while_final_correct.z..."
    timeout 10 ./target/release/zetac test_while_final_correct.z 2>&1 | grep -i "result\|error\|panic"
else
    echo "ERROR: Compiler not built!"
fi
'@

# Run Docker container
docker run --rm -v "${PWD}:/zeta" -w /zeta ubuntu:22.04 bash -c $dockerCommand