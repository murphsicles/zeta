# Capybara DB — Development Notes

## Compiler Bugs Found (bootstrap branch)

### 1. Empty function body → External linkage (confirms old behavior)
- `fn empty() {}` sets External linkage, so linker can't find symbol (U no T)
- `malloc`/`free` are generated with empty bodies → need External to link from C runtime
- **Fix attempted in `codegen.rs:1109`**: emit stub body for all empty functions
  - **Broke**: `malloc`/`free` shadowed C runtime versions
  - **Need**: distinguish builtins (External) from user fns (stub)
  - **Working theory**: check `module_resolver.rs` — it generates `malloc/free` as `FnDef` with empty bodies, should generate as `extern` instead

### 2. Inline module parser (`pub mod X { ... }`)
- Content after first inline module (`pub mod os { ... } pub mod storage { ... } fn main()`) not emitted
- Parser appears to consume past the closing `}` and swallow the rest of the file
- **Workaround**: flat structure (no inline modules) until fixed
- **File**: `src/frontend/parser/top_level.rs` — `parse_mod()` function

### 3. Name mangling inconsistency (`::` vs `__`)
- `gen_mirs` stores functions with `::` separator in module (LLVM symbol)
- Call sites from OTHER modules use `__` (double underscore) before the callee is parsed
- **File**: `src/backend/codegen/codegen.rs` — `get_function()` at line 1579, `gen_mirs()` at line 994
- **Theory**: functions stored as `storage::store_open` in `self.fns`, call site generates `storage__store_open`

### 4. Struct/tuple return value codegen
- Returning structs/tuples from functions produces garbage field values
- Direct field access on local structs works correctly
- **Suspected**: codegen returns pointer to local instead of copying

### 5. Const string to extern C
- `const DATA_DIR: i64 = "/path"` — passing to extern fn gets wrong pointer value
- Literal strings work fine
- **Suspected**: const string creates a local copy, loses the pointer

## Toolchain Setup
```
bin/zetac                       — compiled AOT compiler (v0.11.0)
zeta_runtime_c.o                — C runtime (POSIX wrappers)
/tmp/capy_runtime.o             — custom runtime (posix_open_mode, capy_argc...)
~/.local/bin/gcc (wrapper)      — adds -lzstd and /tmp/capy_runtime.o
```

## Build
```
./build.sh    # assembles os.z + runtime.z + main.z → build.z → compile
/tmp/capybara --replica 0
```
