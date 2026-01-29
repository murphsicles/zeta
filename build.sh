#!/bin/bash
# build.sh: Build Zeta v0.4.0 (Pure Zeta, No Cargo/Rust)
# Run from repo root: ./build.sh
# Assumes zeta binary is in PATH or current dir (from previous build or download)
# Builds new zeta from src/main.z

# Step 1: Compile src/main.z to new binary
./zeta compile src/main.z -o zeta_new

# Step 2: Run tests for validation
./zeta_new run src/tests.z

# Step 3: Benchmark self-compile
time ./zeta_new compile src/main.z -o zeta_bench

# Step 4: Update to new binary
mv zeta_new zeta
rm zeta_bench # Cleanup

echo "Zeta v0.4.0 built successfully!"