#!/bin/bash
# Capybara DB build script
# Assembles multi-file Zeta sources into single-file for zetac compiler
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-/tmp/capybara}"
SRC="$DIR/src"

# Build order: os -> runtime -> main
# Each file is prefixed with its original filename as a comment for traceability
{
  echo "// === os.z ==="
  cat "$SRC/os.z"
  echo ""
  echo "// === runtime.z ==="
  grep -v "^use os;" "$SRC/runtime.z"
  echo ""
  echo "// === ledger.z ==="
  grep -v "^use os;" "$SRC/ledger.z"
  echo ""
  echo ""
  echo "// === vsr.z ==="
  grep -v "^use os;" "$SRC/vsr.z"
  echo ""
  echo "// === main.z ==="
  grep -v "^use os;" "$SRC/main.z"
} > "$DIR/build.z"

echo "=== Assembled build.z ==="
wc -l "$DIR/build.z"

echo "=== Compiling ==="
cd /home/zeta/.openclaw/workspace/zeta
export PATH="/home/zeta/.local/bin:$PATH"
bin/zetac "$DIR/build.z" -o "$OUT" 2>&1
echo "=== Done ==="
