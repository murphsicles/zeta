#!/bin/bash
echo "=== Zeta Compiler WSL Build Test ==="
echo "Time: $(date)"
echo ""

WORKSPACE="/mnt/c/Users/mummy/.openclaw/workspace"
cd "$WORKSPACE"

# Check if Rust is already installed
if command -v cargo &> /dev/null; then
    echo "✓ Rust/cargo already installed"
else
    echo "Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "$HOME/.cargo/env"
fi

# Check if build-essential is installed
if dpkg -l | grep -q build-essential; then
    echo "✓ build-essential already installed"
else
    echo "Installing build-essential..."
    sudo apt-get update && sudo apt-get install -y build-essential
fi

echo ""
echo "Building Zeta compiler..."
cargo build --release

echo ""
echo "Testing compiler..."
if [ -f "./target/release/zetac" ]; then
    echo "✓ Compiler built successfully"
    
    echo ""
    echo "Test 1: Simple while loop (should return 3)"
    cat tests/test_while_final_correct.z
    echo ""
    echo "Running test..."
    timeout 10s ./target/release/zetac tests/test_while_final_correct.z
    WHILE_EXIT=$?
    
    if [ $WHILE_EXIT -eq 124 ]; then
        echo "❌ TEST FAILED: Infinite loop detected (timeout)"
        echo "CRITICAL: While loop bug NOT fixed"
    elif [ $WHILE_EXIT -eq 0 ]; then
        echo "✓ Test completed successfully"
        echo "CRITICAL: While loop bug appears to be FIXED"
    else
        echo "⚠ Test failed with exit code: $WHILE_EXIT"
    fi
    
    echo ""
    echo "Test 2: Murphy's Sieve (should return 25)"
    cat tests/murphy_after_fix.z
    echo ""
    echo "Running test..."
    timeout 30s ./target/release/zetac tests/murphy_after_fix.z
    MURPHY_EXIT=$?
    
    if [ $MURPHY_EXIT -eq 124 ]; then
        echo "❌ TEST FAILED: Infinite loop detected (timeout)"
        echo "CRITICAL: Murphy's Sieve has infinite loop bug"
    elif [ $MURPHY_EXIT -eq 0 ]; then
        echo "✓ Test completed successfully"
        echo "CRITICAL: Murphy's Sieve works - infinite loop bug FIXED"
    else
        echo "⚠ Test failed with exit code: $MURPHY_EXIT"
    fi
    
else
    echo "❌ Compiler build failed"
fi

echo ""
echo "=== Test Complete ==="