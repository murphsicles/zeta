# Zeta: The Final Systems Language

[<img alt="Zeta Logo" width="128px" src="https://z-lang.org/assets/images/z128.png" />](https://z-lang.org)
[![Latest Release](https://img.shields.io/github/v/release/murphsicles/zeta)](https://github.com/murphsicles/zeta/releases)
[![Zeta v1.0.1](https://img.shields.io/badge/Zeta-v1.0.1-8A2BE2)](https://github.com/murphsicles/zeta/releases/tag/v1.0.1)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Zeta is a self-hosted systems programming language targeting LLVM.
Built from the algebraic foundations of Stepanov's *Elements of Programming* — first principles, zero bloat, maximum efficiency.

## v1.0.1 — Pure Zeta

The compiler is written entirely in Zeta. The bootstrap loop is closed:

```
./bin/zetac src/main.z -o zetac
```

One binary, your source file, a target triple. No package manager. No lockfiles.

## Branches

| Branch | Contents | Purpose |
|--------|----------|---------|
| **main** | Pure Zeta source | Self-hosted compiler, production |
| **bootstrap** | Rust source (legacy) | Rust-based compiler for bootstrapping new platforms |
| **dev** | Pure Zeta source | Development branch, feature work |

## Quick Start

```bash
# Clone and use the pre-built binary
git clone https://github.com/murphsicles/zeta.git
cd zeta
./bin/zetac examples/hello.z -o hello
./hello
```

## Build from Source

```bash
# From bootstrap branch (requires Rust + LLVM 21)
git checkout bootstrap
cargo build --release --bin zetac
```

## License

MIT
