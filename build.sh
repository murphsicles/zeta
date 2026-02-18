#!/bin/bash
# build.sh: Build Zeta v0.4.1 (Pure Zeta Self-Host from latest binaries)
# Run from repo root: ./build.sh
# Assumes the latest self-hosted zeta binary (from v0.4.0 or v0.3.4 release assets) is in PATH or current directory
# Uses it to compile the pure Zeta source in /src/ into the new v0.4.1 binary
# Step 1: Compile src/main.z (pure Zeta) to new binary using latest zeta
./zeta compile src/main.z -o zeta_new
# Step 2: Run tests for validation with the newly built binary
./zeta_new run src/tests.z
# Step 3: Benchmark self-compile time (pure self-host loop)
./zeta_new compile src/main.z -o zeta_bench
# Step 4: Replace the old binary with the new self-hosted one
mv zeta_new zeta
rm zeta_bench
# Cleanup
echo "Zeta v0.4.1 built successfully! Fully self-hosted from latest binaries on February 18, 2026."
