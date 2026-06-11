#!/bin/bash
# Capybara DB LSM test build script
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-/tmp/capybara_lsm_test}"
SRC="$DIR/src"

{
  echo "// === os.z ==="
  cat "$SRC/os.z"
  echo ""
  echo "// === xxh64.z ==="
  cat "$SRC/xxh64.z"
  echo ""
  echo "// === ledger.z ==="
  grep -v "^use os;" "$SRC/ledger.z"
  echo ""
  echo "// === runtime.z ==="
  grep -v "^use os;" "$SRC/runtime.z"
  echo ""
  echo "// === bloom.z ==="
  cat "$SRC/bloom.z"
  echo ""
  echo "// === memtable.z ==="
  cat "$SRC/memtable.z"
  echo ""
  echo "// === sstable.z ==="
  cat "$SRC/sstable.z"
  echo ""
  echo "// === manifest.z ==="
  cat "$SRC/manifest.z"
  echo ""
  echo "// === lsm.z ==="
  cat "$SRC/lsm.z"
  echo ""
  echo "// === lsm_test.z ==="
  cat "$SRC/lsm_test.z"
} > "$DIR/build_lsm_test.z"

echo "=== Assembled ==="
wc -l "$DIR/build_lsm_test.z"

echo "=== Compiling ==="
cd /home/zeta/.openclaw/workspace/zeta
export PATH="/home/zeta/.local/bin:$PATH"
bin/zetac "$DIR/build_lsm_test.z" -o "$OUT" 2>&1
echo "=== Done ==="
