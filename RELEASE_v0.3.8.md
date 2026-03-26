# v0.3.8 Release Notes

## Features Implemented

### 1. Float Literal Support
- New `FloatLit(String)` variant in `AstNode` enum
- Supports decimal (`123.456`), scientific (`1.23e-4`), and trailing dot (`123.`) formats
- Implemented by LEX (Code Guru) as Phase 1

### 2. String Escape Sequences
- Enhanced `parse_string_lit` function with full escape support
- Supports: `\"`, `\\`, `\n`, `\t`, `\r`, `\b`, `\f`, `\0`
- Unicode escapes: `\u{1F600}`
- Hex escapes: `\x7F`
- Implemented by LEX (Code Guru) as Phase 1

### 3. Const Parsing Support
- `ConstDef` variant in `AstNode` enum
- Parser support for `const NAME: TYPE = VALUE;` syntax
- Critical for parsing v0.3.7's own source code

### 4. Type Checking Unification
- Hindley-Milner type inference with occurs check
- Algebraic type system foundation
- Implemented by SEM (Semantic Child) as Phase 1

### 5. Inline Operator Optimization
- 60+ redundant external operator declarations removed
- Code generation patterns optimized
- Implemented by GEN (Generative Engine) as Phase 1

### 6. Match Statement Support
- `Match` variant added to `AstNode` enum with `MatchArm` struct
- Parser support for `match expr { pattern => expr, ... }` syntax
- Basic MIR generation (if-else chain for match arms)
- Foundation for full pattern matching in future releases

## Technical Details

**Commit:** $(git rev-parse HEAD)
**Date:** 2026-03-26
**Branch:** v0.3.8

## What's Next

- **v0.3.9**: Enhanced pattern matching (literals, variables, guards)
- **v0.4.0**: Full self-compilation capability
- **v1.0.0**: Pure Zeta bootstrap

## Family Contributions

- **LEX**: Float literals, string escapes
- **SEM**: Type checking unification  
- **GEN**: Inline operator optimization
- **Zak**: Coordination, release management

---
*Dark Factory Lineage - This is the way.*