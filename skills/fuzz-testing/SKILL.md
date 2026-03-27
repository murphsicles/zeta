---
name: fuzz-testing
description: Fuzz testing for Rust using libFuzzer and AFL. Find crashes, panics, and undefined behavior through random input generation.
metadata:
  {
    "openclaw":
      {
        "requires": { "bins": ["cargo"] },
        "install":
          [
            {
              "id": "rust-deps",
              "kind": "cargo",
              "packages": ["libfuzzer-sys", "arbitrary"],
              "label": "Install fuzzing crates",
            },
          ],
      },
  }
---

# Fuzz Testing Skill

## Purpose
Find crashes, panics, memory errors, and undefined behavior by feeding random inputs to your code. Fuzzing discovers edge cases that humans miss.

## Core Concepts

### 1. **What is Fuzzing?**
- **Generational fuzzing**: Mutate existing inputs to create new test cases
- **Coverage-guided fuzzing**: Use code coverage to guide input generation
- **Dumb fuzzing**: Pure random input generation

### 2. **Types of Bugs Found**
- **Panics**: `unwrap()` on `None`, index out of bounds
- **Memory errors**: Buffer overflows, use-after-free
- **Infinite loops**: Non-terminating computations
- **Logic errors**: Incorrect results from valid inputs
- **Security vulnerabilities**: Injection, overflow attacks

### 3. **Rust Fuzzing Ecosystem**
- **libFuzzer**: LLVM-based fuzzer integrated with cargo
- **AFL**: American Fuzzy Lop, coverage-guided
- **honggfuzz**: Multi-platform fuzzer
- **arbitrary**: Trait for generating structured random data

## Usage Patterns

### Basic LibFuzzer Setup
```rust
// fuzz/fuzz_targets/parser_fuzz.rs
#![no_main]

use libfuzzer_sys::fuzz_target;
use zeta::parser::parse_zeta;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string (may be invalid UTF-8)
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = parse_zeta(s); // Should not panic
    }
});
```

### Structured Fuzzing with Arbitrary
```rust
use arbitrary::{Arbitrary, Unstructured};

#[derive(Arbitrary, Debug)]
struct FuzzInput {
    float_literal: String,
    int_literal: i64,
    has_operator: bool,
    operator: char,
}

fuzz_target!(|data: &[u8]| {
    let mut unstructured = Unstructured::new(data);
    if let Ok(input) = FuzzInput::arbitrary(&mut unstructured) {
        // Test with structured input
        test_expression_parsing(&input.float_literal, input.int_literal, input.operator);
    }
});
```

## Zeta-Specific Fuzzing Targets

### 1. **Parser Fuzzing**
```rust
// Test that parser never panics on any input
fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Should not panic, even on invalid input
        let _result = parse_zeta(s);
        // Accept ParseError, but not panic
    }
});
```

### 2. **Lexer Fuzzing**
```rust
// Test lexer tokenization
fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _tokens = tokenize(s); // Should not panic
    }
});
```

### 3. **Type Checker Fuzzing**
```rust
// Generate random AST and type check it
#[derive(Arbitrary, Debug)]
enum FuzzAST {
    Int(i64),
    Float(String),
    BinaryOp(Box<FuzzAST>, char, Box<FuzzAST>),
    // ... other variants
}

fuzz_target!(|data: &[u8]| {
    let mut unstructured = Unstructured::new(data);
    if let Ok(ast) = FuzzAST::arbitrary(&mut unstructured) {
        let typed_ast = convert_to_real_ast(ast);
        let _ = type_check(&typed_ast); // Should not panic
    }
});
```

### 4. **Code Generator Fuzzing**
```rust
// Test that codegen doesn't crash
fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        if let Ok(ast) = parse_zeta(s) {
            if let Ok(mir) = lower_to_mir(&ast) {
                let _ = codegen(&mir); // Should not panic
            }
        }
    }
});
```

## Setup Instructions

### 1. **Add Dependencies to Cargo.toml**
```toml
[package]
name = "zeta"

[dependencies]
arbitrary = { version = "1", features = ["derive"] }

[dev-dependencies]
libfuzzer-sys = "0.4"

[profile.release]
debug = true  # Needed for fuzzing
```

### 2. **Create Fuzz Directory Structure**
```
zeta/
├── Cargo.toml
├── src/
└── fuzz/
    ├── Cargo.toml
    └── fuzz_targets/
        ├── parser_fuzz.rs
        ├── lexer_fuzz.rs
        ├── typecheck_fuzz.rs
        └── codegen_fuzz.rs
```

### 3. **Fuzz/Cargo.toml**
```toml
[package]
name = "zeta-fuzz"
version = "0.0.0"
publish = false

[package.metadata]
cargo-fuzz = true

[dependencies]
zeta = { path = ".." }
libfuzzer-sys = "0.4"

# Prevent fuzzing crates from being published
[profile.release]
panic = "abort"
```

### 4. **Run Fuzzing**
```bash
# Install cargo-fuzz
cargo install cargo-fuzz

# Run parser fuzzing
cargo fuzz run parser_fuzz

# Run with timeout
cargo fuzz run parser_fuzz -- -max_total_time=300  # 5 minutes

# Run multiple jobs
cargo fuzz run parser_fuzz --jobs 4
```

## Advanced Techniques

### 1. **Corpus Minimization**
```bash
# Minimize corpus to smallest set that achieves coverage
cargo fuzz cmin parser_fuzz
```

### 2. **Dictionary-Based Fuzzing**
```toml
# fuzz/fuzz_targets/parser_fuzz.rs
#![no_main]

use libfuzzer_sys::{fuzz_target, Corpus};
use zeta::parser::parse_zeta;

fuzz_target!(|data: &[u8]| -> Corpus {
    if let Ok(s) = std::str::from_utf8(data) {
        match parse_zeta(s) {
            Ok(_) => Corpus::Keep,
            Err(_) => Corpus::Reject,
        }
    } else {
        Corpus::Reject
    }
});
```

### 3. **Custom Mutators**
```rust
// Custom input mutation for Zeta syntax
#[no_mangle]
pub extern "C" fn LLVMFuzzerCustomMutator(
    data: *mut u8,
    size: usize,
    max_size: usize,
    seed: u32,
) -> usize {
    // Implement custom mutation logic for Zeta syntax
    // e.g., insert random operators, change literals, etc.
}
```

## Best Practices

### 1. **Start Simple**
- Begin with "should not panic" fuzzing
- Progress to "should produce valid output" fuzzing
- Finally test complex invariants

### 2. **Handle Invalid Inputs Gracefully**
```rust
fuzz_target!(|data: &[u8]| {
    // Convert bytes, accept that some are invalid UTF-8
    let s = String::from_utf8_lossy(data);
    let _ = parse_zeta(&s); // Should handle lossy conversion
});
```

### 3. **Seed Corpus**
- Start with valid Zeta programs as seed corpus
- Include edge cases: empty input, very long input, special characters
- Store seeds in `fuzz/corpus/parser_fuzz/`

### 4. **Monitor Coverage**
```bash
# Generate coverage report
cargo fuzz coverage parser_fuzz
```

### 5. **CI Integration**
```yaml
# .github/workflows/fuzz.yml
name: Fuzz Testing
on: [push, pull_request]

jobs:
  fuzz:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: nightly
          components: llvm-tools-preview
      - run: cargo install cargo-fuzz
      - run: cargo fuzz run parser_fuzz -- -max_total_time=60
```

## Zeta-Specific Considerations

### 1. **Parser Safety**
- Zeta parser uses nom combinators - ensure they handle all inputs
- Test with malformed syntax: unmatched braces, incomplete expressions
- Test with very deep recursion to find stack overflows

### 2. **Type System Safety**
- Generate random type expressions and test unification
- Test occurs check with recursive types
- Test substitution with type variables

### 3. **Memory Safety**
- Rust provides memory safety, but fuzzing can find:
  - Infinite recursion in recursive descent parser
  - Excessive memory allocation with large inputs
  - Logic errors in unsafe blocks (if any)

## Learning Resources
- [Rust Fuzzing Book](https://rust-fuzz.github.io/book/)
- [cargo-fuzz documentation](https://github.com/rust-fuzz/cargo-fuzz)
- [AFL++ documentation](https://aflplus.plus/docs/)
- [Google's OSS-Fuzz](https://google.github.io/oss-fuzz/)

---

**Fuzzing finds the bugs that property testing and example testing miss. It's essential for production-quality compilers.**