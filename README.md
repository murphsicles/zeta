# [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Zeta — The language that will outlive its bootstrap

[<img alt="Zeta Logo" width="128px" src="https://z-lang.org/assets/images/z128.png" />](https://z-lang.org) [![Latest Release](https://img.shields.io/github/v/release/murphsicles/zeta?label=zeta)](https://github.com/murphsicles/zeta/releases) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Self-Hosting](https://img.shields.io/badge/self--hosting-51%2F51%20files-success)](https://github.com/murphsicles/zeta)

**Zeta is a systems programming language bootstrapped in Rust, targeting LLVM — writing itself in Zeta since v0.9.18.** No lifetimes, no borrow checker overhead, no ceremony. Just first-principles systems programming built from the algebraic foundations of Stepanov's *Elements of Programming*.

> *"Weaponized minimalism. Surgical violence against complexity."* — Roy Murphy

## Why Zeta?

### For Rust developers tired of the ceremony

| Rust | Zeta |
|------|------|
| `fn foo(x: &'a mut Vec<i32>)` | `fn foo(x: &Vec<i64>)` |
| Lifetime annotations everywhere | Simple borrowing — inferred, no annotations needed |
| `Box::new(...)` for heap | `let v = Vec::new()` — heap when you need it |
| `#[derive(Debug)]` boilerplate | Built-in debug printing |
| Separate crate for SIMD | `[i64; 4]` vector type built into the language |
| Procedural macros are extern | CTFE `comptime {}` blocks inline |

```zeta
// Zeta: clean, fast, readable
fn sieve(limit: i64) -> i64 {
    let mut count = 1;   // 2 is prime
    let mut i = 3;
    while i <= limit {
        if is_prime(i) { count += 1; }
        i += 2;
    }
    return count;
}
```

## What Zeta is

A **production-ready systems language** with:

### Zero-compromise compilation
- **AOT** — Compile directly to native executables via LLVM
- **JIT** — REPL and runtime code generation for rapid iteration
- **Self-hosting** — 51/51 source files compiling themselves since v0.9.18

### First-class language features
- **Algebraic data types** — structs, enums, tuples, generics, specialization
- **CTFE** — `comptime {}` blocks execute at compile time, not runtime
- **SIMD** — Vector types (`[i64; 4]`, `[f32; 4]`, etc.) with full LLVM auto-vectorization
- **Async/await** — State machine lowering with thread-local reactor
- **Ownership without lifetimes** — Simple borrowing model, no annotation tax

### Built for real work
- **Direct LLVM IR** — No intermediate C dependency, full optimization control
- **Murphy's Sieve** — Wheel-optimized prime sieving, competition-ready
- **Blockchain primitives** — BSV and Solana support built in

## ✨ Features at a glance

### Core Language
```zeta
// Strong static typing with inference
fn add(x: i64, y: i64) -> i64 { return x + y; }
fn infer() {
    let x = 42;      // inferred i64
    let b = true;    // inferred bool
}

// First-class functions and closures
fn apply(f: (i64) -> i64, x: i64) -> i64 { return f(x); }
fn make_adder(n: i64) -> (i64) -> i64 {
    return fn(x: i64) -> i64 { x + n };
}

// Algebraic data types
struct Point { x: i64, y: i64 }
enum Option<T> { Some(T), None }

// Generics with specialization
fn identity<T>(x: T) -> T { return x; }
fn max<T: Ord>(a: T, b: T) -> T {
    if a >= b { return a; }
    return b;
}
```

### Memory Model — stack by default, heap when you ask
```zeta
fn stack_example() {
    let arr: [i64; 5] = [1, 2, 3, 4, 5];  // stack allocated
}
fn heap_example() {
    let vec = Vec::new();                    // heap allocated
    vec.push(42);
}
fn borrow() {
    let data = Vec::new();
    let len = data.len();      // immutable borrow
    data.push(2);              // mutable borrow (exclusive)
}
```

### Compile-Time Evaluation
```zeta
const FACTORIAL_10: i64 = comptime {
    fn fact(n: i64) -> i64 {
        if n <= 1 { return 1; }
        return n * fact(n - 1);
    }
    fact(10)
};
// FACTORIAL_10 = 3628800 — computed at build time
```

### SIMD — no special syntax needed
```zeta
fn simd_add(a: [i64; 4], b: [i64; 4]) -> [i64; 4] {
    return a + b;  // LLVM auto-vectorizes
}
```

### Standard Library — 20+ modules, ready
`std::mem` · `std::ptr` · `std::cmp` · `std::hash` · `std::iter` · `std::vec` · `std::string` · `std::option` · `std::result` · `std::collections` · `std::simd` · `std::thread` · `std::sync` · `std::io` · `std::fs` · `std::net` · `std::time` · `std::path` · `std::process` · `std::ffi`

## 📁 Layout

```
zeta/
├── bin/              # Pre-built zetac compiler
├── src/              # Self-hosted Zeta sources (51+ files)
│   ├── main.z        # Entry point
│   ├── frontend/     # Lexer, parser, AST
│   ├── middle/       # Resolver, MIR, type system
│   ├── backend/      # Code generation (LLVM)
│   └── runtime/      # Runtime library
├── tests/            # Zeta test suite (44 categories)
├── examples/         # Example programs
└── docs/             # Documentation
```

## 🔧 Get started

```bash
git clone https://github.com/murphsicles/zeta.git
cd zeta

# Compile a Zeta source file
./bin/zetac src/main.z -o zetac_new   # Self-compile the compiler
./bin/zetac examples/hello.z -o hello  # Or compile a program
./hello

# Run tests
./bin/zetac tests/language/basic_tests.z
```

## 🔗 Links

- **GitHub**: https://github.com/murphsicles/zeta
- **Website**: https://z-lang.org
- **Releases**: https://github.com/murphsicles/zeta/releases
- **Bootstrap branch** (Rust source): https://github.com/murphsicles/zeta/tree/bootstrap
- **License**: MIT

---

*Zeta — The language that will outlive its bootstrap.*
