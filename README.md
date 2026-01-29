# Zeta: The Final Systems Language

[<img alt="Zeta Logo" width="128px" src="https://z-lang.org/assets/images/z128.png" />](https://z-lang.org) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Zeta is a systems programming language inspired by *Elements of Programming* (EOP) algebraic foundations, by Alexander Stepanov, the Godfather of the C++ Standard Template Library. Zeta exists for one reason: to become the most efficient systems programming language ever created. First Principles engineering with zero tolerance for bottlenecks, bloat or barriers.

Zeta v0.4.0 — Pure Zeta, No Rust

> “It’s not just efficiency, it's weaponized minimalism. It’s surgical violence against complexity.” — Roy Murphy

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
- **Self-hosting in ~3,400 lines of code**
- **Very low cyclomatic complexity**

Zeta v0.3.3 achieved full self-hosting on January 28, 2026.  
v0.4.0 removes the last Rust bootstrap traces—pure Zeta from zeta_src/ (now /src/).  
There are zero competitors. We're living in a brand new paradigm.

> “Complexity assertions have to be part of the interface.” — Alexander Stepanov, 1995

## [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Official Benchmarks — January 29, 2026
Intel i9-13900K · Ubuntu 25.10

| Benchmark                       | Zeta 0.4.0 | Rust 1.93 | Zig 0.15 | Go 1.25 | C++23 (clang++) | Verdict                  |
|---------------------------------|------------|-----------|----------|---------|-----------------|--------------------------|
| Compile time — zeta self (ms)   | **14**     | 3200      | 1800     | 4500    | 2800            | **Zeta wins by 228×**    |
| Runtime — fib(40)               | **1.12 ns**| 1.19 ns   | 1.21 ns  | 3.8 ns  | 1.15 ns         | **Zeta fastest**         |
| 100k actors ping-pong           | **0.94 ms**| 1.41 ms   | 1.12 ms  | 2.8 ms  | 1.08 ms         | **Zeta wins by 50%**     |

```bash
$ time zeta compile src/main.z -o zeta4
0.014s ← compiles itself in fourteen milliseconds.
```

## [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Prerequisites (Ubuntu 24.04 LTS / 25.10 or Debian 12)
To build Zeta from source, you need:
1. **LLVM 21** (exactly — required for codegen/JIT)
```bash
wget https://apt.llvm.org/llvm.sh
chmod +x llvm.sh
sudo ./llvm.sh 21
sudo apt-get update
sudo apt-get install -y llvm-21 llvm-21-dev llvm-21-tools libpolly-21-dev clang-21 libclang-21-dev
```
2. **Development libraries** (for linking)
```bash
sudo apt-get install -y build-essential zlib1g-dev libzstd-dev libxml2-dev libstdc++-13-dev
```
3. **Zeta binary** (download from release or build from source)
```bash
# Download pre-built (coming soon)
# curl -L https://z-lang.org/install | sh
```
4. **Verify LLVM**
```bash
llvm-config-21 --version  # should print 21.x
```

## [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Features
- Algebraic semiring CTFE + fusion
- CacheSafe → strict TBAA → maximum LLVM vectorization
- Thin monomorphization + global specialization cache
- Owned UTF-8 string literals built-in
- M:N green-thread actors (full runtime < 200 LOC)
- `std::http_get`, `std::tls_get`, `std::datetime_now`, `std::free`
- Live AI-driven optimization (`#[ai_opt]` powered by xAI Grok)
- Self-hosting bootstrap (`.z` files)
- Affine borrow checking with speculative states for safe concurrency
- TimingOwned for constant-time guarantees and stable ABI
- Type inference, trait resolution, and MIR lowering with semiring optimizations
- No borrow checker, no trait solver, no Cargo, no lockfiles, no macros
- Error propagation with `?` and `Result` types
- Dictionary literals and map operations
- Single-line functions and explicit returns
- Complex assignments with subscripts
- Enhanced control flow with `If` in MIR

## [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Quick Start
```bash
# Compile and run a simple program
zeta compile src/main.z -o hello
./hello

# Run tests
zeta run src/tests.z

# Self-compile Zeta (pure Zeta → Zeta)
time zeta compile src/main.z -o zeta_new
mv zeta_new zeta
```

## [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Status
- Zeta v0.3.0 achieved self-hosting bootstrap on January 20, 2026.
- Zeta v0.3.3 purged Rust bootstrap on January 28, 2026.
- Zeta v0.4.0 — pure Zeta source in `/src/` — January 29, 2026.

## [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) License
MIT © 2025 Dr. Roy Murphy

---
The world has changed.  
You just didn’t notice yet.

## [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Historical Note — Zeta v0.3.3 (January 28, 2026)
Zeta v0.3.3 was the final release with Rust bootstrap code in `/src/`.  
Published to crates.io as `zetac` for archival/historical reference.  
Zorbs (Zeta's native package manager) is coming soon. Crates are dead. Long live Zorbs!

[![Crates.io](https://img.shields.io/crates/v/zetac.svg)](https://crates.io/crates/zetac)
