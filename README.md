# [<img alt="Zeta Logo" width="24px" src="https://z-lang.org/assets/images/z72.png" />](https://z-lang.org) Zeta: The Final Systems Language

[<img alt="Zeta Logo" width="128px" src="https://z-lang.org/assets/images/z128.png" />](https://z-lang.org) [![Latest Release](https://img.shields.io/github/v/release/murphsicles/zeta)](https://github.com/murphsicles/zeta/releases) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Zeta is a systems programming language inspired by Elements of Programming (EOP) algebraic foundations, by Alexander Stepanov, the Godfather of the C++ Standard Template Library. Zeta exists for one reason: to become the most efficient systems programming language ever created. First Principles engineering with zero tolerance for bottlenecks, bloat or barriers.

> "It's not just efficiency, it's weaponized minimalism. It's surgical violence against complexity." - Roy Murphy

- **Insane efficiency**
- **Unbeatable execution speed & performance**
- **Built for next-gen AI infrastructure**
- **Designed for machine learning & numerical analysis**
- **Perfect for scientific computation**
- **Awesome for embedded hardware**
- **Military grade security**
- **Runs faster than traditional systems languages**
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

Zeta v0.6.0 delivers a competition-ready compiler with comprehensive test suite and professional project structure. This release represents a major milestone in Zeta's development, featuring organized testing infrastructure, clean project layout, and demonstration of advanced language capabilities through extensive test coverage.

## ðŸš€ Zeta v0.6.0 - Competition Ready

**ðŸŽ¯ RELEASE v0.6.0 - "Professional Structure & Comprehensive Testing"**

### ðŸ† **Competition Preparation**
- **Clean Project Structure**: Professional organization for competition judges
- **Comprehensive Test Suite**: 100+ test cases demonstrating Zeta capabilities
- **Language Coverage**: Full feature testing (arithmetic, control flow, functions, arrays)
- **Algorithm Validation**: Verified implementations of advanced algorithms
- **Performance Benchmarks**: Competition-ready timing and optimization tests
- **CTFE Implementation**: Compile-Time Function Evaluation infrastructure with evaluator, context, and value systems

### ðŸ“ **Project Organization**
```
zeta/
â”œâ”€â”€ src/              # Zeta source examples
â”œâ”€â”€ tests/            # Organized test suite (Zeta code)
â”‚   â”œâ”€â”€ language/     # Language feature tests
â”‚   â”œâ”€â”€ algorithms/   # Algorithm implementations
â”‚   â”œâ”€â”€ competition/  # Competition demonstrations
â”‚   â”œâ”€â”€ performance/  # Benchmark framework
â”‚   â””â”€â”€ README.md     # Test documentation
â”œâ”€â”€ showcase/         # Showcase examples
â”œâ”€â”€ docs/             # Documentation
â”œâ”€â”€ .github/          # GitHub workflows
â””â”€â”€ README.md         # This file
```

### ðŸ§ª **Test Suite Highlights**
- **Language Features**: Arithmetic, control flow, functions, arrays, types
- **Algorithms**: Murphy's Sieve, QuickSort, mathematical algorithms
- **Competition**: 30030-wheel algorithm, performance benchmarks
- **Verification**: Correctness validation for all features

### ðŸš€ **Getting Started**
```bash
# Clone the repository
git clone https://github.com/murphsicles/zeta.git
cd zeta

# Use the pre-built compiler binary (Windows)
bin/zetac.exe showcase/ctfe_demo.z -o demo.out
./demo.out

# Explore Zeta source examples
cat src/arithmetic_operators_test.z
```

## ðŸ“š **Language Features**

Zeta is designed for maximum efficiency with a clean, minimal syntax:

### Basic Syntax Example
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

### Advanced Features
- **Strong Static Typing**: Type inference with safety guarantees
- **Efficient Memory Management**: Stack-based with optional heap allocation
- **SIMD Optimization**: Built-in vector operations
- **Competition-Ready**: Optimized for performance benchmarks

## ðŸ—ï¸ **Project Structure**

Zeta v0.6.0 features a clean, professional structure:

### **Source Code (`src/`)**
- Zeta source examples demonstrating language features
- Example programs for learning and testing
- Organized by feature category

### **Test Suite (`tests/`)**
- Organized by category (language, algorithms, competition)
- Comprehensive coverage of all features
- Example implementations for learning

### **Documentation (`docs/`)**
- API reference and usage guides
- Performance benchmarks
- Competition preparation materials

### **Showcase (`showcase/`)**
- Example programs demonstrating Zeta capabilities
- Performance comparison examples
- Real-world use cases

## ðŸ”§ **Building & Running**

### **Requirements**
- Windows (or compatible platform for the pre-built binary)
- Git (for cloning the repository)

### **Quick Start**
```bash
# Clone the repository
git clone https://github.com/murphsicles/zeta.git
cd zeta

# Use the pre-built compiler binary (Windows)
bin/zetac.exe showcase/ctfe_demo.z -o demo.out
./demo.out

# Compile and run other Zeta programs
bin/zetac.exe src/arithmetic_operators_test.z -o test.out
./test.out
```

### **Running Tests**
```bash
# Compile and run Zeta test files
bin/zetac.exe tests/simple.z -o test.out
./test.out

# Explore the test suite
ls tests/
```

### **Advanced Usage**
The `zetac.exe` compiler supports the following options:
- `-o <output>`: Specify output executable name
- `--help`: Show help message
- `--version`: Show compiler version

For more examples, see the `showcase/` directory.

## ðŸ¤ **Contributing**

Zeta welcomes contributions! See `CONTRIBUTING.md` for guidelines.

## ðŸ“„ **License**

Zeta is licensed under the MIT License - see the `LICENSE` file for details.

## ðŸ”— **Links**
- **Website**: https://z-lang.org
- **GitHub**: https://github.com/murphsicles/zeta
- **Documentation**: https://docs.z-lang.org
- **Community**: Discord (link in GitHub README)

