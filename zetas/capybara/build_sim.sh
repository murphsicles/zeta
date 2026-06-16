#!/bin/bash
# Capybara DB simulation build script — Phase 4
# Concatenates modules into one file (compiler limitation: no inline modules yet)
# Order matters: each file can call functions defined in earlier files.
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-/tmp/capybara_sim}"
SRC="$DIR/src"
SIM="$SRC/sim"

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
  echo "// === sim_runner.z ==="
  cat "$SIM/sim_runner.z"
  echo ""
  echo "// === sim_generator.z ==="
  cat "$SIM/sim_generator.z"
  echo ""
  echo "// === sim_checker.z ==="
  cat "$SIM/sim_checker.z"
  echo ""
  echo "// === sim_main.z ==="
  cat "$SIM/sim_main.z"
} > "$DIR/build_sim.z"

echo "=== Assembled build_sim.z ==="
wc -l "$DIR/build_sim.z"

echo "=== Compiling sim binary ==="
cd /home/zeta/.openclaw/workspace/zeta
export PATH="/home/zeta/.local/bin:$PATH"
bin/zetac "$DIR/build_sim.z" -o "$OUT" 2>&1
echo "=== Done ==="
