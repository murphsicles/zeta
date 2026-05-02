# [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Zeta: The Final Systems Language

[<img alt="Zeta Logo" width="128px" src="https://z-lang.org/assets/images/z128.png" />](https://z-lang.org) [![Latest Release](https://img.shields.io/github/v/release/murphsicles/zeta)](https://github.com/murphsicles/zeta/releases) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Zeta is a systems programming language bootstrapped in Rust, targeting LLVM. Built from the algebraic foundations of Stepanov's *Elements of Programming* — first principles, zero bloat, maximum efficiency.

> "It's not just efficiency, it's weaponized minimalism. Surgical violence against complexity." — Roy Murphy

## 🚀 v0.10.0 — Bootstrap Maturity

**51 self-hosted Zeta source files compile through the Zeta compiler itself.**

The north star is full self-hosting: Zeta compiling Zeta, then shedding the Rust bootstrap entirely. Every release brings us closer. The compiler is written in Rust today, but its core semantic analysis, MIR, and code generation paths are being re-expressed in Zeta itself — one module at a time.

### What's Shipped

- **Self-compiling stdlib passes** — all 51 `zeta_src/` files parse, typecheck, and lower through the Zeta pipeline
- **Standard Library Tier 1** — `std::mem`, `std::ptr`, `std::cmp`, `std::hash`, `std::iter`, `std::vec`, `std::string`, and more
- **Stable ABI groundwork** — persistent TBAA metadata, alignment-preserving type layout, SIMD vector ABI
- **Full MIR pipeline** — from AST → HIR → THIR → MIR, with specialized lowering for each IR tier
- **Competition-ready Murphy's Sieve** — prime sieving with wheel factorization for PrimeZeta

### 📁 Project Layout

```
zeta/
├── zeta_src/          # Self-hosted Zeta compiler sources (51 files)
├── src/               # Rust bootstrap compiler
│   ├── frontend/      # Lexer, parser, AST
│   ├── middle/        # Resolver, MIR, type system, specialization
│   ├── backend/       # LLVM codegen, JIT, monomorphization
│   └── runtime/       # Actor runtime, scheduler, channels
├── build/stubs/       # Generated stdlib stubs (bootstrap-critical, tracked)
├── tests/             # Zeta test suite (44+ categories)
├── docs/              # Language documentation
├── examples/          # Showcase examples
└── Cargo.toml         # Rust bootstrap build
```

### 🔧 Building

```bash
# Clone and build the Rust bootstrap compiler
git clone https://github.com/murphsicles/zeta.git
cd zeta
cargo build --release

# Compile Zeta source through itself
./target/release/zetac zeta_src/main.z

# Run tests
cargo test
```

### 📝 Zeta by Example

```zeta
// Murphy's Sieve — wheel-optimized prime counting
fn count_primes(limit: i64) -> i64 {
    if limit < 2 { return 0; }
    let mut count: i64 = 1;  // 2 is prime
    let mut i: i64 = 3;
    while i <= limit {
        if is_prime(i) { count += 1; }
        i += 2;
    }
    return count;
}

fn main() -> i64 {
    let primes = count_primes(1000000);
    println_i64(primes);  // Output: 78498
    return 0;
}
```

### 🏛️ Architecture

```
Zeta Source (.z)
    │
    ▼ Lexer → Parser
    │
    ▼ AST → HIR
    │
    ▼ Resolver (imports, generics, specialization)
    │
    ▼ THIR (typed HIR)
    │
    ▼ MIR (mid-level IR — CFG with basic blocks)
    │
    ▼ LLVM IR → Machine Code
```

### 🔗 Links

- **GitHub**: https://github.com/murphsicles/zeta
- **Website**: https://z-lang.org
- **License**: MIT

---

*Zeta: The Final Systems Language — built to outlive its bootstrap.*
