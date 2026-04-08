# MAC-KNOWLEDGE.md - Metaprogramming Architect Knowledge Cache

## Current State Analysis (2026-04-08)

### 1. CTFE Infrastructure (EXISTS)
- Location: `src/middle/ctfe/`
- Files: `mod.rs`, `context.rs`, `error.rs`, `value.rs`
- Evaluator: `evaluator.rs` (in zeta-github directory)
- Status: Partially implemented, supports basic constant evaluation

### 2. Macro System (MINIMAL)
- AST has `MacroCall` variant
- Basic macro definitions in `tests.z` (assert_eq, assert)
- No macro expansion infrastructure found
- No hygienic macro system

### 3. Integration Points Needed:
- **With SYN**: Macro syntax parsing (needs enhancement)
- **With SEM**: Type checking for macro expansions
- **With GEN**: Code generation for macro output
- **With VER**: Macro testing framework

## Immediate Implementation Plan:

### Phase 1: Macro Expansion Foundation
1. Create `src/middle/macros/` directory
2. Implement `MacroExpander` struct
3. Add macro registry and expansion logic
4. Support basic pattern matching

### Phase 2: Declarative Macros (`macro_rules!` style)
1. Implement pattern-based macro expansion
2. Add metavariables (`$ident`, `$expr`, `$ty`, etc.)
3. Support repetition (`$(...)*`, `$(...)+`, `$(...)?`)
4. Implement hygienic macro system

### Phase 3: CTFE Enhancement
1. Extend existing CTFE evaluator
2. Add `const fn` support
3. Implement compile-time computation
4. Add constant propagation

### Phase 4: Attribute Macros
1. Basic attribute macro system
2. Derive macro foundation
3. Custom attribute processing

## Git Discipline:
- Branch: `dev-mac`
- Commit messages: `[MAC] Description`
- Hourly pushes to GitHub
- CI must pass