# Sprint: Zero C Dependency — Full Zeta Runtime

**Goal:** Eliminate C source from Zeta's bootstrap path.
**Timeline:** Evening session, ~6 hours.
**Release track:** v0.9.18 → v0.9.22

---

## Phase 1: Zeta Runtime Core (v0.9.18)

**Rewrite `runtime.c` functions as Zeta source in `zeta_src/runtime/runtime.z`**

Build a `zeta_src/runtime/runtime.z` that provides all runtime functions:

| Function | C Impl | Zeta Strategy |
|---|---|---|
| `println_i64` | `printf` | `extern "C" fn write(fd: i64, buf: *, len: i64)` → syscall wrapper |
| `print_str` | `printf` | Same write-based approach |
| `get_time_us` | `clock_gettime` | `extern "C" fn clock_gettime(...)` |
| `runtime_malloc` | `malloc` | `extern "C" fn malloc(size: i64) → *mut u8` |
| `runtime_free` | `free` | `extern "C" fn free(ptr: *mut u8)` |
| `host_result_is_ok` | trivial | Pure Zeta |
| `clone_i64` | identity | Pure Zeta |
| `amp` | identity | Pure Zeta |
| Everything trivial | stub | Pure Zeta |

**Strategy:** The system calls (malloc, write, clock_gettime) still go through `extern "C"` — linking against libc.so. But the ZETA source for the runtime is in Zeta, not C. The only C left is libc itself (system library, not our code).

**Deliverable:** `zeta_src/runtime/runtime.z` with 90+ functions, compiled by `--compile-zeta`.

## Phase 2: Compile Runtime Into Bootstrap (v0.9.19)

**Modify `--compile-zeta` pipeline to link compiled Zeta runtime.**

Currently `--compile-zeta` produces `.o` + `gcc` linking against `zeta_runtime_c.o`. Change it to:
1. Compile `zeta_src/runtime/runtime.z` (along with all other files)
2. The runtime functions are now in the SAME object file as the compiler functions
3. No separate `zeta_runtime_c.o` needed for core functions

**Changes needed:**
- `main.rs` compile pipeline: after --compile-zeta, skip linking against `zeta_runtime_c.o`
- If linking still fails (external libc symbols), link against `-lc` only (system libc, not our C source)

**Deliverable:** `zetac --bootstrap` flag that produces a self-contained `.o` with no C runtime dependency.

## Phase 3: Pure Zeta AOT Linker (v0.9.20)

**Replace `gcc` linking step with Zeta-owned linking.**

The AOT path currently runs `gcc output.o -o output -lc -no-pie`. Replace with:
- Collect all undefined symbols from the `.o`
- Resolve them against the Zeta runtime (which is now in the same `.o`)
- If any remain, they're libc syscalls (`malloc`, `write`, `exit`) — use `extern "C"` declarations that resolve against libc.so at runtime via `dlopen`/`dlsym` or just link against `-lc` as a system library

**Alternative (simpler):** Remove the `zeta_runtime_c.o` check. If it doesn't exist, link only against `-lc`. All Zeta runtime functions are already in the `.o`.

**Deliverable:** `zetac` can AOT-compile to a standalone binary without `zeta_runtime_c.o` present.

## Phase 4: Self-Hosted LLVM Codegen (v0.9.21-22)

**The big one — zeta_src/ codegen becomes the active backend.**

`zeta_src/backend/codegen/` contains LLVM IR generation in Zeta. The pipeline:
1. Rust pipeline: parse → typecheck → MIR → (call Zeta codegen) → LLVM module
2. The Zeta codegen produces the LLVM IR string
3. Rust writes the IR to a file and calls `llc` to compile to `.o`
4. Link with AOT linker (from Phase 3)

**This is the self-hosting threshold:** The Rust compiler only parses, typechecks, and lowers to MIR. Codegen is in Zeta.

**Deliverable:** `zetac --self-host` flag that uses zeta_src/ codegen instead of Rust codegen.

---

## Release Cadence

| Phase | Tag | Est. Time | Clear Win |
|---|---|---|---|
| 1 | v0.9.18 | 2h | No C source in repo |
| 2 | v0.9.19 | 1h | `--bootstrap` produces standalone binary |
| 3 | v0.9.20 | 1h | No `zeta_runtime_c.o` needed |
| 4 | v0.9.21-22 | 2h | Self-hosted codegen active |

Total: ~6 hours of focused work.
