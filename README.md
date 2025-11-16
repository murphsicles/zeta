# 🚀 Zeta: Next-Gen Systems Language

[![Crates.io](https://img.shields.io/crates/v/zeta.svg)](https://crates.io/crates/zeta)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Zeta is a systems programming language inspired by *Elements of Programming* (EOP) algebraic concepts (semirings, semimodules, semigroups). It addresses Rust's pain points: compilation time < Go, execution perf > Rust/Zig, AI-optimized codegen, memory-safe (affine borrowck + speculative exec), no GC. Bootstrapped in Rust 2024, LLVM backend.

## ✨ Features
- **Algebraic Core**: Semiring CTFE, fusion (assoc_fold peephole).
- **Safety**: Affine ownership, TimingOwned constant-time, CacheSafe races.
- **Concurrency**: Send/Sync actors, fault-tolerant channels.
- **Ergonomics**: Hybrid traits (nominal+structural), auto-derive (Copy/Eq), thin templates.
- **Opts**: MLGO vectorize/branch pred, xAI Grok integration.
- **Std Embeds**: http/TLS/datetime + serde/rand/log/governor/prometheus (intrinsics).
- **Bootstrap**: Self-hosting Zeta compiler (.z files).

## ⚡ Quick Start

```bash
cargo run -- examples/add.zeta  # JIT exec
cargo build --release           # LLVM bin
```

## 🔨 Build
- Rust 2024 edition.
- Deps: nom, inkwell, rayon, chrono, reqwest, serde, criterion.
- `cargo test` for e2e (EOP algos, actors, perf stubs).

## 📊 Status
See [plan.rs](src/plan.rs): v0.0.1-alpha (full compiler, xAI ready).

## 📄 License
MIT © 2025 Dr. Roy Murphy
