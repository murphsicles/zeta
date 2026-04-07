#!/bin/bash
# Build and test Zeta in Docker

echo "=== Building Zeta Compiler in Docker ==="
echo ""

# Build the compiler
docker run --rm -v "$(pwd):/zeta" -w /zeta ubuntu:22.04 bash -c "
    echo 'Installing dependencies...'
    apt-get update && apt-get install -y curl build-essential 2>/dev/null
    
    echo 'Installing Rust...'
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    export PATH=\"/root/.cargo/bin:\$PATH\"
    
    echo 'Building Zeta compiler...'
    cargo build --release 2>&1 | tail -10
    
    echo ''
    echo '=== Testing While Loop Fix ==='
    if [ -f target/release/zetac ]; then
        echo 'Running test_while_final_correct.z...'
        timeout 30 ./target/release/zetac test_while_final_correct.z 2>&1 | grep -i 'result\|error\|panic' || echo 'No output (might be hanging if bug not fixed)'
    else
        echo 'ERROR: Compiler build failed!'
    fi
"

echo ""
echo "=== Docker Build Complete ==="