# [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Zeta v1.0.0 — The Foundational Release

[<img alt="Zeta Logo" width="128px" src="https://z-lang.org/assets/images/z128.png" />](https://z-lang.org) [![Latest Release](https://img.shields.io/github/v/release/murphsicles/zeta)](https://github.com/murphsicles/zeta/releases) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Zeta is a systems programming language bootstrapped in Rust, targeting LLVM.** v1.0.0 marks the foundational release — pure Zeta, self-hosting ready, with everything needed to build on top.

Built from the algebraic foundations of Stepanov's *Elements of Programming* — first principles, zero bloat, maximum efficiency.

> "Weaponized minimalism. Surgical violence against complexity." — Roy Murphy

## 🚀 v1.0.0 — Pure Zeta Foundation

This release strips away the Rust bootstrap scaffolding. The compiler binary ships in `bin/`. The `zeta_src/` directory contains 51 self-hosted Zeta source files that compile through the Zeta pipeline. Everything after this builds on a pure Zeta foundation.

## ✨ Features

### Core Language

**Strong static typing** with type inference:
```zeta
fn add(x: i64, y: i64) -> i64 {
    return x + y;
}

fn infer() {
    let x = 42;       // inferred i64
    let y = 3.14;     // inferred f64
    let b = true;     // inferred bool
}
```

**First-class functions and closures:**
```zeta
fn apply(f: (i64) -> i64, x: i64) -> i64 {
    return f(x);
}

fn make_adder(n: i64) -> (i64) -> i64 {
    return fn(x: i64) -> i64 { x + n };
}
```

**Algebraic data types** (structs, enums, tuples):
```zeta
struct Point {
    x: i64,
    y: i64,
}

enum Option<T> {
    Some(T),
    None,
}

fn origin() -> Point {
    return Point { x: 0, y: 0 };
}
```

**Generics and specialization:**
```zeta
fn identity<T>(x: T) -> T {
    return x;
}

fn max<T: Ord>(a: T, b: T) -> T {
    if a >= b { return a; }
    return b;
}
```

### Memory Model

**Stack-priority allocation** with optional heap:
```zeta
fn stack_example() {
    let arr: [i64; 5] = [1, 2, 3, 4, 5];  // stack allocated
    let sum = arr[0] + arr[1];
}

fn heap_example() {
    let vec = Vec::new();                    // heap allocated
    vec.push(42);
    vec.push(100);
}
```

**Ownership and borrowing:**
```zeta
fn borrow_example() {
    let data = Vec::new();
    data.push(1);
    
    let len = data.len();      // immutable borrow
    let first = data[0];       // immutable borrow
    
    data.push(2);              // mutable borrow (unique)
}
```

### Standard Library (Tier 1)

| Module | Description | Status |
|--------|-------------|--------|
| `std::mem` | Memory operations, size_of, align_of | ✅ |
| `std::ptr` | Raw pointer operations | ✅ |
| `std::cmp` | Ordering, comparison traits | ✅ |
| `std::hash` | Hashing infrastructure | ✅ |
| `std::iter` | Iterator traits and adapters | ✅ |
| `std::vec` | Vector type with dynamic sizing | ✅ |
| `std::string` | UTF-8 string type | ✅ |
| `std::option` | Option enum | ✅ |
| `std::result` | Result enum | ✅ |
| `std::collections` | Collection types | ✅ |
| `std::simd` | SIMD vector operations | ✅ |
| `std::thread` | Threading primitives | ✅ |
| `std::sync` | Synchronization primitives | ✅ |
| `std::io` | Input/Output | ✅ |
| `std::fs` | Filesystem operations | ✅ |
| `std::net` | Networking | ✅ |
| `std::time` | Time and duration | ✅ |
| `std::path` | Path manipulation | ✅ |
| `std::process` | Process management | ✅ |
| `std::char` | Character operations | ✅ |
| `std::marker` | Marker types | ✅ |
| `std::ffi` | Foreign function interface | ✅ |

### Compiler Pipeline

```
Zeta Source (.z)
    │
    ▼ Lexer → Parser
    │
    ▼ AST → HIR (High-level IR)
    │
    ▼ Resolver (imports, generics, specialization)
    │
    ▼ THIR (Typed HIR)
    │
    ▼ MIR (Mid-level IR — CFG with basic blocks)
    │
    ▼ LLVM IR → Machine Code
```

### Compile-Time Evaluation (CTFE)

Execute Zeta code at compile time:
```zeta
const FACTORIAL_10: i64 = comptime {
    fn fact(n: i64) -> i64 {
        if n <= 1 { return 1; }
        return n * fact(n - 1);
    }
    fact(10)
};
// FACTORIAL_10 = 3628800 at runtime
```

### SIMD Vector Types

```zeta
fn simd_add(a: [i64; 4], b: [i64; 4]) -> [i64; 4] {
    return a + b;  // auto-vectorized where possible
}
```

### Murphy's Sieve — Prime Sieving

Competition-ready wheel-optimized prime counting:
```zeta
fn murphy_sieve(limit: i64) -> i64 {
    if limit < 2 { return 0; }
    
    // 30030-wheel: 80.8% reduction in checks
    const WHEEL: i64 = 30030;  // 2×3×5×7×11×13
    
    let mut count: i64 = 1;  // 2 is prime
    let mut i: i64 = 3;
    
    while i <= limit {
        if is_coprime_to_wheel(i) {
            if is_prime(i) { count += 1; }
        }
        i += 2;
    }
    return count;
}
```

## 📁 Project Layout

```
zeta/
├── bin/              # Pre-built compiler binary
│   └── zetac         # v1.0.0 Linux x86-64
├── zeta_src/         # Self-hosted Zeta sources (51 files)
│   ├── main.z        # Entry point
│   ├── frontend/     # Lexer, parser, AST
│   ├── middle/       # Resolver, MIR, type system
│   ├── backend/      # Code generation
│   └── runtime/      # Runtime library
├── build/stubs/      # Generated stdlib stubs (bootstrap)
├── tests/            # Zeta test suite (44+ categories)
├── examples/         # Example programs
├── docs/             # Documentation
└── README.md         # This file
```

## 🔧 Getting Started

```bash
# Clone the repository
git clone https://github.com/murphsicles/zeta.git
cd zeta

# Compile a Zeta source file (Linux)
./bin/zetac zeta_src/main.z -o output

# Or compile an example
./bin/zetac examples/hello.z -o hello
./hello

# Run tests
./bin/zetac tests/language/basic_tests.z
```

### From Source (Rust Bootstrap)

```bash
cargo build --release
./target/release/zetac zeta_src/main.z
```

## 📝 Examples

### Hello, Zeta
```zeta
fn main() -> i64 {
    println_str("Hello, Zeta!");
    return 0;
}
```

### Fibonacci
```zeta
fn fib(n: i64) -> i64 {
    if n <= 1 { return n; }
    return fib(n - 1) + fib(n - 2);
}

fn main() -> i64 {
    let result = fib(20);
    println_i64(result);  // Output: 6765
    return 0;
}
```

### Working with Vectors
```zeta
fn main() -> i64 {
    let v = Vec::new();
    v.push(10);
    v.push(20);
    v.push(30);
    
    let sum: i64 = v[0] + v[1] + v[2];
    println_i64(sum);  // Output: 60
    return 0;
}
```

## 🏗️ Architecture

The compiler pipeline processes Zeta source through multiple IR tiers:

1. **Lexing & Parsing** — Tokenizes and builds AST from `.z` files
2. **HIR** — High-level IR: resolves imports, identities, and macros
3. **Resolver** — Type resolution, generics, specialization, concept checking
4. **THIR** — Typed HIR: fully resolved types and identities
5. **MIR** — Mid-level IR: control flow graph with basic blocks
6. **LLVM Codegen** — Lowers MIR to LLVM IR, runs optimization passes
7. **Machine Code** — LLVM produces native executable

## 🔗 Links

- **GitHub**: https://github.com/murphsicles/zeta
- **Website**: https://z-lang.org
- **License**: MIT

---

*Zeta v1.0.0 — The language that will outlive its bootstrap.*
