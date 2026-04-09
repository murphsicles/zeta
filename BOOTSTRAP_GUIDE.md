# Zeta Bootstrap Guide

## Overview

This guide explains the Zeta bootstrap chain and the mixed repository structure that preserves the ability to compile Zeta from source without external dependencies.

## The Bootstrap Problem

A self-hosting compiler faces a fundamental challenge: how do you compile the compiler without already having a compiler? Zeta solves this through a carefully preserved bootstrap chain:

1. **Rust v0.3.7**: The final Rust-based Zeta compiler
2. **Zeta v0.3.8**: The first Zeta-compiled version (compiled by Rust v0.3.7)
3. **Zeta v0.4.x**: Iterative improvements (compiled by previous Zeta versions)
4. **Zeta v0.5.0**: Pure Zeta milestone (compiled by Zeta v0.4.1)

## Repository Structure

This repository maintains a **mixed implementation** to preserve the bootstrap chain:

```
zeta/
├── src/                    # Rust source code (original compiler)
├── zeta_src/              # Pure Zeta source code
│   ├── backend/           # Backend components
│   ├── frontend/          # Frontend components  
│   ├── middle/            # Middle-end components
│   └── runtime/           # Runtime components
├── Cargo.toml             # Rust build configuration
├── zetac.exe              # Linux Zeta compiler binary
└── ...                    # Other files
```

## Why Mixed Implementation?

The mixed structure serves several critical purposes:

1. **Bootstrap Preservation**: Maintains the Rust→Zeta compilation chain
2. **Historical Continuity**: Preserves the complete development history
3. **Fallback Option**: Provides Rust source as a backup compilation method
4. **Educational Value**: Shows the evolution from Rust to pure Zeta

## Building from Source

### Option 1: Using the Pre-built Compiler (Recommended)

1. Download `zetac.exe` from the v0.5.0 GitHub release
2. Use it to compile Zeta source files:
   ```bash
   ./zetac.exe compile zeta_src/main.z -o zeta_compiler
   ```

### Option 2: Using the Rust Bootstrap Chain

1. Build the Rust compiler:
   ```bash
   cargo build --release
   ```
2. Use the Rust-built compiler to compile Zeta:
   ```bash
   ./target/release/zetac compile zeta_src/main.z -o zeta_compiler
   ```
3. The resulting binary is a pure Zeta compiler

### Option 3: Full Bootstrap from Scratch

1. Start with Rust v0.3.7 (tag `v0.3.7-final-bootstrap`)
2. Compile Zeta v0.3.8 source
3. Use Zeta v0.3.8 to compile Zeta v0.4.1
4. Use Zeta v0.4.1 to compile Zeta v0.5.0

## Release Process

The automated release workflow (`.github/workflows/release-zeta.yml`) handles:

1. **Build**: Compiles Zeta source using the bootstrap chain
2. **Test**: Runs test suite to ensure correctness
3. **Package**: Creates release artifacts (compiler binary + source tarball)
4. **Release**: Publishes to GitHub Releases with detailed notes

## Verification

To verify the bootstrap chain integrity:

```bash
# Check that Rust can compile Zeta
cargo run -- compile zeta_src/main.z -o test_compiler

# Check that the resulting compiler works
./test_compiler --version

# Compile Zeta source with the Zeta compiler
./test_compiler compile zeta_src/main.z -o final_compiler

# Verify the final compiler is pure Zeta
./final_compiler --version
```

## Future Development

With v0.5.0 released, future development focuses on:

1. **Cross-platform support**: Windows and macOS builds
2. **Enhanced documentation**: Comprehensive language guide
3. **Performance improvements**: Continued optimization
4. **Community engagement**: Issue templates, contribution guides

## Troubleshooting

### Common Issues

1. **LLVM version mismatch**: Ensure LLVM 21 is installed
2. **Rust toolchain**: Use Rust nightly 2024 edition
3. **Permission errors**: Ensure executables have execute permissions
4. **Missing dependencies**: Install development libraries (zlib, zstd, etc.)

### Getting Help

- Check `BUILD_INSTRUCTIONS.md` for detailed build steps
- Review `DECISION_LOG.md` for design decisions
- Consult `WORK_QUEUE.md` for current status and next actions
- File issues on GitHub for bugs or questions

## Conclusion

The Zeta bootstrap chain represents a significant achievement in compiler engineering. By maintaining a mixed repository structure, we preserve the ability to build Zeta from source while showcasing the pure Zeta implementation. This approach ensures long-term sustainability and educational value for the compiler development community.