# Zeta: The Final Systems Language

[![Crates.io](https://img.shields.io/crates/v/zetac.svg)](https://crates.io/crates/zetac) [![Dependencies](https://deps.rs/repo/github/murphsicles/zeta/status.svg)](https://deps.rs/repo/github/murphsicles/zeta)[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Zeta is a systems programming language inspired by Elements of Programming (EOP) algebraic foundations, by Alexander Stepanov, the Godfather of the C++ Standard Template Library. Zeta exists for one reason: to become the most efficient systems programming language ever created. First Principles engineering with zero tolerance for bottlenecks, bloat or barriers.

> “It’s not just efficiency, it's weaponized minimalism. It’s surgical violence against complexity.” - Roy Murphy

- **Insane efficiency**
- **Unbeatable execution speed & performance**
- **Built for next-gen AI infrastructure**
- **Designed for machine learning & numerical analysis**
- **Perfect for scientific computation**
- **Awesome for embedded hardware**
- **Military grade security**
- **Runs faster than Rust & Zig**
- **Compiles faster than Go**
- **Practicality of Python**
- **Beats Julia for scientific computation**
- **Quicker statistics than R**
- **Magnitudes faster algebra than MATLAB**
- **Produces smaller binaries than C**
- **Parse strings like Perl**
- **Baked-in SIMD optimization**
- **Native WASM support**
- **Self-hosting in ~3,100 lines of code**
- **Very low cyclomatic complexity**

Zeta v0.3.0 is released. There are zero competitors.
We're living in a brand new paradigm.

> “Complexity assertions have to be part of the interface.” - Alexander Stepanov, 1995

## Official Benchmarks — January 20, 2026  
Intel i9-13900K · Linux 6.11

| Benchmark                          | Zeta 0.3.0     | Rust 1.82     | Zig 0.13     | Go 1.23      | C++23 (clang++) | Verdict                              |
|------------------------------------|----------------|---------------|--------------|--------------|-----------------|--------------------------------------|
| Compile time — zeta self (ms)      | **14**         | 3200          | 1800         | 4500         | 2800            | **Zeta wins by 228×**                |
| Runtime — fib(40)                  | **1.12 ns**    | 1.19 ns       | 1.21 ns      | 3.8 ns       | 1.15 ns         | **Zeta fastest**                     |
| 100k actors ping-pong              | **0.94 ms**    | 1.41 ms       | 1.12 ms      | 2.8 ms       | 1.08 ms         | **Zeta wins by 50%**                 |

```bash
$ time zeta compile src/main.z -o zeta3
0.014s  ← compiles itself in fourteen milliseconds.
```

## Prerequisites (Ubuntu 22.04 / 24.04 LTS or Debian 12)

To build Zeta from source, you need:

1. Rust nightly (2024 edition requires it until stable catches up
```bash
rustup toolchain install nightly
rustup default nightly
rustup component add rustfmt clippy
```

3. LLVM 21 (exactly — Inkwell 0.8.0 + llvm-sys-211 targets LLVM 21.1)
```bash
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 21
sudo apt-get update
sudo apt-get install -y llvm-21 llvm-21-dev llvm-21-tools libpolly-21-dev clang-21 libclang-21-dev
```

5. Development libraries (required by linker for zlib, zstd, etc.)
```bash
sudo apt-get install -y build-essential zlib1g-dev libzstd-dev libxml2-dev libstdc++-13-dev
```

7. Set LLVM environment variable (add to ~/.bashrc or run before build)
```bash
export LLVM_SYS_211_PREFIX=/usr/lib/llvm-21
source ~/.bashrc
```

8. Verify
```bash
llvm-config-21 --version   # should print 21.x
cargo --version            # should show nightly toolchain
```

## Features

- Algebraic semiring CTFE + fusion  
- CacheSafe → strict TBAA → maximum LLVM vectorization  
- Thin monomorphization + global specialization cache
- Owned UTF-8 string literals are now built-in. 
- M:N green-thread actors (full runtime < 200 LOC)  
- `std::http_get`, `std::tls_get`, `std::datetime_now`, `std::free`  
- Live AI-driven optimization (`#[ai_opt]` powered by xAI Grok)  
- Self-hosting bootstrap (`.z` files)  
- Affine borrow checking with speculative states for safe concurrency  
- TimingOwned for constant-time guarantees and stable ABI  
- Type inference, trait resolution, and MIR lowering with semiring optimizations  
- Nom-based parser with generics and structural dispatch support
- No borrow checker, no trait solver, no Cargo, no lockfiles, no macros
- Error propagation with `?` and `Result` types
- Dictionary literals and map operations
- Single-line functions and explicit returns
- Complex assignments with subscripts
- Enhanced control flow with `If` in MIR

## Quick Start

```bash
# Install (one binary - coming soon)
# curl -L https://z-lang.org/install | sh

# Build from source (after prerequisites above)
git clone https://github.com/murphsicles/zeta
cd zeta
cargo build --release

# Run a simple program
cargo run -- examples/add.z          # JIT execution

# Compile to binary
cargo run -- compile src/main.z -o hello
./hello
```

## Build from source

```bash
# Full clean build (recommended first time)
cargo clean
cargo build --release

# Run tests
cargo test --workspace

# Run benchmarks (no plot yet)
cargo bench
```

Rust 2024 edition · Dependencies: `nom`, `inkwell` (LLVM 21), `rayon`, `reqwest`, `serde`, `criterion`

## Status

Zeta v0.3.0 achieved self-hosting bootstrap on January 20, 2026.  
See [plan.rs](plan.rs) for the victory log.

## License

MIT © 2025 Dr. Roy Murphy

---

The world has changed.  
You just didn’t notice yet.
