#!/bin/bash
# Source Rust environment
source /home/zeta/.cargo/env

cd /mnt/c/Users/mummy/.openclaw/workspace

echo "=== Testing Zeta Compiler in WSL ==="
echo "Rust version: $(rustc --version)"
echo "Cargo version: $(cargo --version)"
echo ""

# First, try to build without system dependencies
echo "Attempting to build Zeta compiler..."
echo "Note: This may fail if system C compiler is missing"
cargo build --release 2>&1 | tail -20

echo ""
echo "If build fails due to missing C compiler, we need to install gcc."
echo "However, apt-get update is timing out."
echo ""
echo "Alternative approach: Test with existing Windows binary via Wine?"
echo "Or install minimal C compiler from Ubuntu base image?"