# [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Zeta — The final systems language

[<img alt="Zeta Logo" width="128px" src="https://z-lang.org/assets/images/z128.png" />](https://z-lang.org) [![Latest Release](https://img.shields.io/github/v/release/murphsicles/zeta?label=zeta)](https://github.com/murphsicles/zeta/releases) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT) [![Self-Hosting](https://img.shields.io/badge/self--hosting-51%2F51%20files-success)](https://github.com/murphsicles/zeta)

**Zeta is a next-generation systems language built on the algebraic foundations of Alexander Stepanov's *Elements of Programming*.** Zero bloat, maximum efficiency, first principles engineering from the ground up. A compiler that writes itself, targeting LLVM with full AOT and JIT capability.

> *"Weaponized minimalism. Surgical violence against complexity."* — Roy Murphy

## Built on First Principles

Zeta is engineered from the algebraic bedrock. Every feature traces back to fundamental mathematical structures — semigroups, monoids, rings, and their compositions. This isn't academic: it produces measurable wins in code size, cache behavior, and compilation speed.

```zeta
// Semiring folds — fold over addition and multiplication simultaneously
// in a single pass, using the algebraic structure to eliminate
// intermediate computation.
fn dot_product(a: &[i64], b: &[i64]) -> i64 {
    return semiring_fold(a, b, 0, 1, add, mul);
}
```

## The Memory System

Zeta's memory model is **stack-priority by default, explicit when you need the heap**. No garbage collector, no reference counting overhead, no lifetime annotation tax.

```zeta
// Stack allocation — zero overhead
fn process() {
    let data: [i64; 1024] = [0; 1024];
    let result = compute(&data);
}

// Heap when you need it — explicit, no surprises
fn build_buffer() -> &Vec<i64> {
    let buf = Vec::new();
    buf.reserve(100_000);
    return buf;
}
```

The compiler eliminates allocations it can prove unnecessary. When you allocate, you know exactly why and where.

## Algebraic Type System

Generics built on concepts, not constraints. Specialization that composes. An identity system that tracks capabilities through the type graph.

```zeta
// Concept-constrained generics
fn sorted_insert<T: Ordered>(list: &List<T>, item: T) {
    let pos = list.position(|x| x >= item);
    list.insert(pos, item);
}

// Specialized implementations — the compiler picks the right one
fn zero<T>() -> T;
// specializations resolve at monomorphization time
```

## Compile-Time Evaluation (CTFE)

Run Zeta code at compile time. Full interpreter with access to the same semantics as runtime code. Compute once, embed the result.

```zeta
const CRC32_TABLE: [u64; 256] = comptime {
    let table: [u64; 256] = [0; 256];
    var i = 0;
    while i < 256 {
        var crc = i;
        var j = 0;
        while j < 8 {
            if crc & 1 != 0 {
                crc = (crc >> 1) ^ 0xEDB88320;
            } else {
                crc = crc >> 1;
            }
            j += 1;
        }
        table[i] = crc;
        i += 1;
    }
    table
};
// CRC32_TABLE is embedded in the binary — zero runtime cost
```

## SIMD — Vectorized by Default

Full SIMD support built into the type system. LLVM auto-vectorization where possible, explicit vectors when you need control.

```zeta
// Explicit vector types — compiler maps directly to SIMD registers
fn vec_add(a: [i64; 4], b: [i64; 4]) -> [i64; 4] {
    return a + b;
}

fn horizontal_sum(v: [i64; 4]) -> i64 {
    // LLVM lowers to hadd or equivalent
    return v[0] + v[1] + v[2] + v[3];
}
```

## Error System

Structured, recoverable, and zero-cost when unused. No unwinding overhead — the compiler knows which paths are clean.

```zeta
fn read_config(path: &str) -> Result<Config, Error> {
    let file = fs::open(path)?;
    let data = file.read_all()?;
    return Config::parse(data);
}

// Pattern match on errors — exhaustive, checked at compile time
fn main() -> i64 {
    match read_config("/etc/zeta.conf") {
        Result::Ok(cfg) => run(cfg),
        Result::Err(e) => {
            log_error("config", e);
            return 1;
        }
    }
}
```

## Logging & Diagnostics

Zero-cost logging that compiles away in release builds. Structured diagnostics with 175+ error codes, each with explanatory text. The compiler tells you what went wrong and where — not just that something failed.

## Zorbs Package Manager

Built-in package management. Dependencies, workspaces, version resolution, and integrated build system. No external tooling required.

```zeta
// package.zorba — declare dependencies once
package "my_app" {
    version = "1.0.0"
    dep "zeta/std" = "1.0"
    dep "zeta/net" = "1.0"
}
```

## Self-Hosting Pipeline

Zeta compiles itself. The bootstrap compiler (written in Rust) builds the Zeta compiler (written in Zeta), which then compiles everything else. Every release tightens the loop.

```
Rust bootstrap  →  Zeta compiler (.z sources)  →  zetac binary
                      ↑ compiles itself ↺
```

51 source files, all self-compiling. Each release reduces the Rust footprint.

## Where Zeta Excels

- **Embedded systems** — deterministic allocation, no runtime, minimal binary footprint
- **High-frequency trading** — predictable latency, SIMD pipelines, cache-conscious data layout
- **Streaming systems** — zero-copy buffers, backpressure-aware channels, async reactor
- **Databases** — CTFE-compiled query plans, tight memory control, lock-free primitives
- **LLM and agentic systems** — efficient tensor pipelines, hardware-aware dispatch, minimal inference overhead
- **Automation infrastructure** — fast compile times, self-hosting reduces toolchain dependencies, repeatable builds

## The Compiler Pipeline

```
Zeta Source (.z)  →  Lexer → Parser → AST → HIR → Resolver → THIR → MIR → LLVM IR → Machine Code
                                                    ↕
                                              CTFE Interpreter
```

Seven IR tiers, each with a specific purpose. The MIR (Mid-level IR) is a control-flow graph with basic blocks — explicit, inspectable, optimizable. The LLVM backend handles register allocation, scheduling, and target-specific lowering.

## Quick Start

```bash
git clone https://github.com/murphsicles/zeta.git
cd zeta

# Self-compile the compiler
./bin/zetac src/main.z -o zetac_new

# Compile and run a program
./bin/zetac examples/hello.z -o hello
./hello
```

## Project Layout

```
zeta/
├── bin/              # Pre-built zetac compiler
├── src/              # Self-hosted Zeta sources (51+ files)
│   ├── main.z        # Entry point
│   ├── frontend/     # Lexer, parser, AST
│   ├── middle/       # Resolver, MIR, type system
│   ├── backend/      # LLVM code generation
│   └── runtime/      # Runtime library
├── tests/            # Zeta test suite
├── examples/         # Example programs
└── docs/             # Documentation, specifications
```

## Links

- **GitHub**: https://github.com/murphsicles/zeta
- **Website**: https://z-lang.org
- **Releases**: https://github.com/murphsicles/zeta/releases
- **Bootstrap source**: https://github.com/murphsicles/zeta/tree/bootstrap
- **License**: MIT

---

*Zeta — The final systems language. Built from first principles. Proved by algebra.*
