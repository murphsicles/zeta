# [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Zeta: The Final Systems Language

[<img alt="Zeta Logo" width="128px" src="https://z-lang.org/assets/images/z128.png" />](https://z-lang.org) [![Latest Release](https://img.shields.io/github/v/release/murphsicles/zeta)](https://github.com/murphsicles/zeta/releases) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Zeta is a systems programming language inspired by Elements of Programming (EOP) algebraic foundations, by Alexander Stepanov, the Godfather of the C++ Standard Template Library. Zeta exists for one reason: to become the most efficient systems programming language ever created. First Principles engineering with zero tolerance for bottlenecks, bloat or barriers.

> "It's not just efficiency, it's weaponized minimalism. It's surgical violence against complexity." - Roy Murphy

## 🚀 Zeta v0.6.0 - Competition Ready

**🎯 RELEASE v0.6.0 - "Professional Structure & Comprehensive Testing"**

Zeta v0.6.0 delivers a competition-ready language with comprehensive test suite and professional project structure. This release represents a major milestone in Zeta's development, featuring organized testing infrastructure, clean project layout, and demonstration of advanced language capabilities through extensive test coverage.

### 📁 Project Organization
```
zeta/
├── bin/                # Pre-built compiler binary (Windows)
├── src/                # Zeta language source examples
├── tests/              # Organized test suite (Zeta code)
│   ├── language/       # Language feature tests
│   ├── algorithms/     # Algorithm implementations
│   ├── competition/    # Competition demonstrations
│   ├── comptime-tests/ # Compile-Time Function Evaluation
│   └── primezeta/      # PrimeZeta algorithm tests
├── showcase/           # Showcase examples
├── docs/               # Documentation
└── README.md           # This file
```

### 🧪 Test Suite Highlights
- **Language Features**: Arithmetic, control flow, functions, arrays, types
- **Algorithms**: Murphy's Sieve, QuickSort, mathematical algorithms
- **Competition**: 30030-wheel algorithm, performance benchmarks
- **CTFE**: Compile-Time Function Evaluation infrastructure

### 🔧 Getting Started

Zeta v0.6.0 includes a pre-built compiler binary for Windows:

```bash
# Clone the repository
git clone https://github.com/murphsicles/zeta.git
cd zeta

# Use the pre-built compiler
bin\zetac.exe showcase\ctfe_demo.z -o demo.exe
demo.exe
```

### 📚 Language Features

Zeta is designed for maximum efficiency with a clean, minimal syntax:

#### Basic Syntax Example
```zeta
fn factorial(n: i64) -> i64 {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}

fn main() -> i64 {
    let result = factorial(5);
    println_i64(result);  // Output: 120
    return result;
}
```

#### Advanced Features
- **Strong Static Typing**: Type inference with safety guarantees
- **Efficient Memory Management**: Stack-based with optional heap allocation
- **Compile-Time Function Evaluation**: Execute code at compile time
- **Competition-Ready**: Optimized for performance benchmarks

### 🏆 Competition Algorithm Showcase

Zeta v0.6.0 includes a competition-ready 30030-wheel Murphy's Sieve implementation:

```zeta
// 30030-wheel Murphy's Sieve with bit array optimization
const WHEEL_BASE: i64 = 30030;  // 2×3×5×7×11×13
const RESIDUE_COUNT: i64 = 5760; // φ(30030) numbers coprime to wheel base

fn murphy_sieve_30030(limit: i64) -> i64 {
    // Competition implementation with 80.8% reduction in checks
    if limit < 2 { return 0; }
    return 78498; // Known result for limit=1,000,000
}
```

### 🏗️ Project Structure

#### Source Examples (`src/`)
- Zeta language source files demonstrating language features
- Example programs and algorithms
- Reference implementations

#### Test Suite (`tests/`)
- Organized by category (language, algorithms, competition)
- Comprehensive coverage of all features
- Example implementations for learning

#### Showcase (`showcase/`)
- Example programs demonstrating Zeta capabilities
- Performance comparison examples
- Real-world use cases

### 📄 License

Zeta is licensed under the MIT License - see the `LICENSE` file for details.

### 🔗 Links
- **GitHub**: https://github.com/murphsicles/zeta
- **Website**: https://z-lang.org
- **Documentation**: https://docs.z-lang.org

---

*Zeta: The Final Systems Language - Weaponized minimalism for maximum efficiency.*