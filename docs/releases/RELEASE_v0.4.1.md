# Zeta v0.4.1 Release Notes

**Release Date:** April 11, 2026  
**Tag:** v0.4.1  
**Status:** STABLE - Stdin Support & REPL Error Recovery  
**Previous Version:** v0.4.0  
**Next Version:** v0.4.2  

## 🎯 Release Overview

v0.4.1 enhances Zeta's I/O capabilities with stdin support (read_i64, read_line) and improves the REPL experience with better error recovery and interactive development features.

## ✨ Key Features

### 1. **Stdin Input Support**
- **read_i64()** function for integer input
- **read_line()** function for string input
- **Interactive program support** for user input
- **Type-safe input parsing** with error handling

### 2. **REPL Enhancements**
- **Error recovery** in interactive mode
- **Better line editing** and history
- **Improved diagnostics** for REPL errors
- **Session persistence** options

### 3. **Developer Experience**
- **Interactive debugging** capabilities
- **Better error messages** for input operations
- **REPL productivity features**
- **Enhanced development workflow**

### 4. **I/O Completeness**
- **Complete input/output ecosystem**
- **Consistent error handling** across I/O operations
- **Cross-platform compatibility**
- **Performance-optimized implementations**

## 🐛 Bug Fixes

- Fixed stdin buffer management issues
- Resolved REPL error recovery edge cases
- Addressed input parsing errors
- Corrected interactive session bugs

## 📊 Performance Improvements

- **Input operations**: Optimized buffering
- **REPL responsiveness**: Improved performance
- **Memory usage**: Efficient buffer management
- **Startup time**: Faster REPL initialization

## 🧪 Testing Status

- **Comprehensive I/O tests** for all input functions
- **REPL integration tests** for interactive use
- **Cross-platform testing** for compatibility
- **Performance benchmarks** meeting targets

## 🔧 Technical Details

### Input Functions
- **read_i64()**: Reads a 64-bit integer from stdin
- **read_line()**: Reads a line of text from stdin
- **Error handling**: Proper validation and recovery
- **Buffer management**: Efficient memory usage

### REPL System
- **Error recovery**: Continue after errors
- **Line editing**: Improved user interface
- **History management**: Session persistence
- **Diagnostics**: Better error reporting

## 🚀 Getting Started

```bash
# Example: Reading input
let age = read_i64();
println("You are " + age + " years old");

let name = read_line();
println("Hello, " + name + "!");

# Using the REPL
$ zetac --repl
> let x = 42;
> println(x);
42
> let y = x * 2;
> println(y);
84
```

## 📚 Documentation Updates

- Complete I/O API documentation
- REPL user guide and tutorial
- Input function examples and patterns
- Error handling guide for I/O operations

## 🤝 Community Contributions

- Thanks to I/O feature request contributors
- Special recognition for REPL testing
- Appreciation for documentation improvements
- Recognition for bug reports and feedback

## 🔮 Future Directions

- Enhanced REPL with tab completion
- More input data types (floats, booleans)
- Better interactive debugging
- REPL script execution

## 📋 Changelog

For a complete list of changes, see the git history between v0.4.0 and v0.4.1.

---

*Zeta: The Final Systems Language - Weaponized minimalism for maximum efficiency.*