#!/bin/bash
# Capybara DB simulation build script
# Standalone binary for deterministic simulation testing
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-/tmp/capybara_sim}"
SRC="$DIR/src"

{
  echo "// === os.z ==="
  cat "$SRC/os.z"
  echo ""
  echo "// === xxh64.z ==="
  cat "$SRC/xxh64.z"
  echo ""
  echo "// === runtime.z ==="
  grep -v "^use os;" "$SRC/runtime.z"
  echo ""
  echo "// === ledger.z ==="
  grep -v "^use os;" "$SRC/ledger.z"
  echo ""
  echo "// === sim.z ==="
  cat "$SRC/sim.z"
} > "$DIR/build_sim.z"

echo "=== Assembled build_sim.z ==="
wc -l "$DIR/build_sim.z"

echo "=== Compiling sim binary ==="
cd /home/zeta/.openclaw/workspace/zeta
export PATH="/home/zeta/.local/bin:$PATH"
bin/zetac "$DIR/build_sim.z" -o "$OUT" 2>&1
echo "=== Done ==="
