# v0.8.0 — CTFE Sprint: Sieve, Recursion, For-Loops & Compound Ops

This release supercharges the Zeta CTFE (Compile-Time Function Evaluation) engine. The compiler can now compute π(1,000,000) = 78,498 at compile time in under 1 second using a Sieve of Eratosthenes.

## CTFE Performance

| Algorithm | Limit | Compile Time | Speedup |
|-----------|-------|-------------|---------|
| Trial division (v0.7.0) | 100,000 | 1.45s | — |
| **Sieve (v0.8.0)** | **100,000** | **0.11s** | **13×** |
| **Sieve (v0.8.0)** | **1,000,000** | **0.90s** | **competitive** |

## What's New

### 🔥 CTFE Engine Overhaul
- **In-place array mutation** — No more cloning the entire 100K-element array per write. O(1) vs O(n). The big one.
- **`IntArray` compact storage** — Auto-converts `[1; 100001]` to packed `Vec<i64>` (8 bytes/element vs ~32 bytes for enum)
- **Scope-less loop bodies** — While loops no longer allocate a HashMap per iteration
- **Recursive comptime functions** — Fixed parameter binding bug (args shadowing prior args in same call). Recursion depth limit: 1024.
- **For loops in CTFE** — `for i in 1..=limit { }` works in comptime functions with range syntax
- **Compound assignments** — `+=`, `-=`, `*=`, `/=`, `%=` in comptime code

### 🐛 Bug Fixes
- Fixed `eval_user_function_call` being shadowed by a stub that returned "CTFE not fully implemented" — comptime function calls now actually execute at compile time
- Fixed `ExprStmt` not handled in `eval_const_expr` (blocking recursive calls in if-else branches)
- Fixed if-expression else branch evaluation (was only evaluating `else_.last()`, dropping earlier statements)
- Fixed JIT execution engine symbol resolution — `run` command works end-to-end

### 🧹 Cleanup
- Removed per-iteration scope allocation from while loops
- Removed unnecessary `in_const_fn` guard that blocked eager CTFE in main()
- Added `body_has_let_decls` heuristic — only allocates scope when `let` is present
- Solution package (`solution_1/`) updated with latest compiler

## Breaking Changes
None. Backward compatible with v0.7.0 comptime code.

## Competition Ready
The Prime Drag Race entry now compiles π(1,000,000) via CTFE in under 1 second. The resulting binary is ~17KB with no sieve runtime code — just `println_i64(78498)`.
