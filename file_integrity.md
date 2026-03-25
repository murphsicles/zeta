# FILE INTEGRITY SYSTEM

## Problem
LLM file operations can truncate files when:
1. Using `read` with `limit` parameter
2. Not reading entire files before writing
3. Hallucinating file content

## Solution
For each file copy operation:
1. **Read entire file** using `Get-Content -Raw`
2. **Verify size and line count** before/after
3. **Test compilation** after each addition
4. **Keep backups** of original files

## Current Status
- ✅ `advanced_generics.rs` - Full copy verified (16,662 bytes, 456 lines)
- ✅ v0.3.6 builds cleanly with this file

## Next Files to Copy
1. `trait_extensions.rs`
2. `ast_extensions.rs`
3. `llvm_extensions_simple.rs`
4. `llvm_advanced.rs`
5. `macro_system.rs`
6. `unsafe_operations.rs`
7. `phase3_integration.rs`
8. `codegen_integration.rs`

## Verification Steps for Each File
1. Check source file exists and has content
2. Read entire file with `-Raw`
3. Write to destination
4. Verify byte and line count match
5. Run `cargo check` to ensure build still works
6. Document results