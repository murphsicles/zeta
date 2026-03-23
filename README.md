# Zeta Compiler

A compiler for the Zeta programming language, written in Zeta itself (bootstrapped).

## Current Status

**v0.3.7** (March 23, 2026) - Bootstrap compiler

### What Works:
- Basic Zeta syntax parsing
- Simple type checking
- Code generation to LLVM
- REPL and file compilation

### Known Limitations:
- v0.3.7 cannot parse the full bootstrap source (`src/main.z`)
- Only 635/5300 characters of `src/main.z` parse successfully
- Generic types (`lt(Result, i64)`) not supported
- Advanced features from bootstrap source not available

## Development Strategy

### Phase 1: Generic Type Support
**Goal:** Extend v0.3.7 to parse `lt(Result, i64)`

**Current capability:** v0.3.7 fails on parenthesized types (`MyType()`, `lt(Result, i64)`)

**Approach:** Incremental extension of v0.3.7's parser

## Project Structure

```
src/
├── frontend/              # Lexical and syntactic analysis
│   ├── lexer/            # Tokenization
│   ├── parser/           # Parsing (parser.z, expr.z, stmt.z)
│   ├── ast/              # Abstract syntax tree
│   └── diagnostics/      # Error reporting
├── middle/               # Semantic analysis
│   ├── mir/              # Mid-level IR
│   ├── resolver/         # Name resolution
│   ├── types/            # Type checking
│   └── ownership/        # Borrow checking
├── backend/              # Code generation
│   └── codegen/          # LLVM code generation
├── driver/               # Compiler driver
└── runtime/              # Runtime support
```

## Getting Started

### Prerequisites
- Windows (currently)
- LLVM (for code generation)
- v0.3.7 compiler (`zetac-v0.3.7-fixed.exe`)

### Building
The compiler is bootstrapped. Current bootstrap chain:
1. v0.3.7 (C++/Rust) → compiles basic Zeta
2. Extended v0.3.7 (goal) → compiles more of bootstrap
3. Eventually: full bootstrap compiler

### Testing
```bash
# Test with v0.3.7
.\zetac-v0.3.7-fixed.exe src\baseline_test.z
```

## Public Development

All development is public and transparent:
- GitHub: https://github.com/murphsicles/zeta
- CI verification on every commit
- Incremental, verifiable progress

## License

Copyright 2025-2026 Roy Murphy

See LICENSE for details.