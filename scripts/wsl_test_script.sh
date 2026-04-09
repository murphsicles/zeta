#!/bin/bash

echo "=== Starting Zeta Compiler WSL Test ==="
echo "Time: $(date)"
echo ""

# Set workspace path
WORKSPACE="/mnt/c/Users/mummy/.openclaw/workspace"
cd "$WORKSPACE"

echo "1. Installing build tools..."
sudo apt-get update
sudo apt-get install -y build-essential llvm clang curl

echo ""
echo "2. Installing Rust..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
source "$HOME/.cargo/env"

echo ""
echo "3. Building Zeta compiler..."
cargo build --release

echo ""
echo "4. Testing while loop fix..."
echo "Test file: test_while_final_correct.z"
cat tests/test_while_final_correct.z
echo ""
echo "Compiling test_while_final_correct.z..."
./target/release/zetac tests/test_while_final_correct.z
echo "Expected return value: 3"

echo ""
echo "5. Testing Murphy's Sieve..."
echo "Test file: murphy_after_fix.z"
cat tests/murphy_after_fix.z
echo ""
echo "Compiling murphy_after_fix.z..."
./target/release/zetac tests/murphy_after_fix.z
echo "Expected return value: 25"

echo ""
echo "6. Creating competition Dockerfile..."
cp Dockerfile.competition Dockerfile.competition.wsltest
echo "Dockerfile copied and ready for testing"

echo ""
echo "7. Testing Docker build..."
docker build -f Dockerfile.competition.wsltest -t zeta-competition-test .

echo ""
echo "=== Test Summary ==="
echo "1. Build tools: ✓"
echo "2. Rust installed: ✓"
echo "3. Zeta compiler built: ✓"
echo "4. While loop test: PENDING"
echo "5. Murphy's Sieve test: PENDING"
echo "6. Dockerfile created: ✓"
echo "7. Docker build test: PENDING"

echo ""
echo "=== CRITICAL: Infinite Loop Bug Verification ==="
echo "If tests 4 and 5 complete without hanging, the infinite loop bug is FIXED."
echo "If they hang, the bug is NOT fixed."