# Building Zeta from Source

## Prerequisites

### For Linux/macOS:
1. Download the Zeta compiler binary (v0.4.1 required for bootstrapping):
   ```bash
   # Using curl
   curl -L -o zetac https://github.com/murphsicles/zeta/releases/download/v0.4.1/zetac
   chmod +x zetac
   
   # Or using wget
   wget -O zetac https://github.com/murphsicles/zeta/releases/download/v0.4.1/zetac
   chmod +x zetac
   ```

### For Windows:
1. Download the Zeta compiler (v0.4.1 required for bootstrapping):
   ```powershell
   # Using PowerShell
   Invoke-WebRequest -Uri "https://github.com/murphsicles/zeta/releases/download/v0.4.1/zetac.exe" -OutFile "zetac.exe"
   ```

## Building Zeta

### Simple Test Compilation
```bash
# Test the compiler
./zetac --version

# Compile a simple test file
./zetac src/fixed_simple_test.z -o test_program
```

### Building the Full Compiler
```bash
# Compile the main compiler (if self-hosting is supported)
./zetac src/main.z -o zetac_new

# Or compile all source files
./zetac src/*.z -o zeta_compiler
```

## Project Structure

```
src/
├── main.z                    # Main compiler entry point
├── frontend/                 # Parser and lexer
├── middle/                   # Type system and MIR
├── backend/                  # Code generation
└── runtime/                  # Standard library
```

## Testing

### Running Tests
```bash
# Compile and run test programs
./zetac tests/*.z

# Or use the test runner if available
./run_tests.sh
```

## Troubleshooting

### Common Issues

1. **Compiler not found or not executable**
   - Ensure the compiler binary has execute permissions: `chmod +x zetac`
   - Check that the binary is compatible with your OS/architecture

2. **Source file parsing errors**
   - Verify Zeta syntax compatibility with compiler version
   - Check for missing dependencies or imports

3. **Build failures**
   - Ensure all source files are present (72 .z files)
   - Check compiler version compatibility

## Next Steps

Once you have a working Zeta compiler built from source:

1. **Self-hosting test**: Try compiling the compiler with itself
2. **Performance testing**: Benchmark against previous versions
3. **Feature development**: Add new language features or optimizations

## Contributing

See CONTRIBUTING.md for guidelines on contributing to Zeta development.