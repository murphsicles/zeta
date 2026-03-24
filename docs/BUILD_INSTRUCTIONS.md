# 🔧 Zeta v0.5.0 Build Instructions

## **Prerequisites**

### **Required:**
- **Rust 1.92.0+** (install from [rustup.rs](https://rustup.rs/))
- **LLVM 21.1+** (with development headers)
- **Cargo** (comes with Rust)
- **Git** (for cloning repository)

### **Optional (for full features):**
- **CMake 3.20+** (for some dependencies)
- **Python 3.8+** (for build scripts)
- **Visual Studio Build Tools** (Windows) or **Xcode Command Line Tools** (macOS)

---

## **📦 Quick Start (Linux/macOS)**

```bash
# 1. Clone repository
git clone https://github.com/murphsicles/zeta.git
cd zeta

# 2. Checkout v0.5.0
git checkout v0.5.0

# 3. Build compiler
cargo build --release

# 4. Install (optional)
cargo install --path .
```

---

## **🪟 Windows Build**

### **Method 1: Using Rust MSVC (Recommended)**
```powershell
# 1. Install Rust with MSVC toolchain
rustup default stable-msvc

# 2. Clone and build
git clone https://github.com/murphsicles/zeta.git
cd zeta
git checkout v0.5.0
cargo build --release

# 3. The compiler will be at:
#    target\release\zetac.exe
```

### **Method 2: Using GNU Toolchain**
```bash
# 1. Install Rust with GNU toolchain
rustup default stable-gnu

# 2. Install MinGW-w64
#    Download from: https://www.mingw-w64.org/

# 3. Build
cargo build --release
```

---

## **🔍 Verifying Your Build**

```bash
# Check compiler version
./target/release/zetac --version

# Test compilation
./target/release/zetac compile tests/unit/test_v0.5.0_complete.z

# Run a simple test
./target/release/zetac run examples/hello.z
```

Expected output for `zetac --version`:
```
zetac 0.5.0 (The Efficiency Release)
```

---

## **⚡ Building with Optimizations**

Zeta v0.5.0 includes advanced optimization systems. To enable them:

### **1. Enable SIMD Match Optimization**
The match optimizer is automatically enabled. It analyzes match patterns and chooses:
- **SIMD vectorization** for dense integer matches
- **Jump tables** for very dense ranges  
- **Binary search** for sparse values
- **Standard match** as fallback

### **2. Enable Cache-Optimized HashMap**
```zeta
// In your code, use:
use zeta::runtime::stdlib::collections::hashmap_optimized::CacheOptimizedHashMap;

let mut map = CacheOptimizedHashMap::<i32, String>::new();
```

### **3. Enable Memory Pooling**
```zeta
// Memory pools are automatically used for:
// - Small allocations (≤ 2KB)
// - Thread-local allocations
// - Temporary buffers
```

---

## **🐳 Docker Build**

```dockerfile
# Dockerfile for Zeta
FROM rust:1.92-slim

# Install LLVM
RUN apt-get update && apt-get install -y \
    llvm-21-dev \
    clang-21 \
    cmake \
    git \
    && rm -rf /var/lib/apt/lists/*

# Clone and build
RUN git clone https://github.com/murphsicles/zeta.git /zeta
WORKDIR /zeta
RUN git checkout v0.5.0
RUN cargo build --release

# Set entry point
ENTRYPOINT ["/zeta/target/release/zetac"]
```

Build and run:
```bash
docker build -t zeta:0.5.0 .
docker run --rm zeta:0.5.0 --version
```

---

## **🔨 Advanced Build Options**

### **Build with Debug Symbols**
```bash
cargo build --release --debug
```

### **Build for Specific Target**
```bash
# Linux x86_64
cargo build --release --target x86_64-unknown-linux-gnu

# macOS Apple Silicon
cargo build --release --target aarch64-apple-darwin

# Windows x86_64
cargo build --release --target x86_64-pc-windows-msvc
```

### **Build with Custom Features**
```bash
# Enable all optimizations (default)
cargo build --release --features "full"

# Minimal build
cargo build --release --no-default-features

# Specific feature set
cargo build --release --features "simd,pooling,hashmap-optimized"
```

---

## **🚨 Troubleshooting**

### **Common Issues:**

#### **1. LLVM Not Found**
```
error: could not find system LLVM
```
**Solution:** Install LLVM development packages:
- **Ubuntu/Debian:** `sudo apt-get install llvm-21-dev libclang-21-dev`
- **Fedora:** `sudo dnf install llvm-devel clang-devel`
- **macOS:** `brew install llvm@21`
- **Windows:** Download LLVM from [llvm.org](https://llvm.org/)

#### **2. Linker Errors**
```
error: linking with `link.exe` failed
```
**Solution (Windows):** Install Visual Studio Build Tools or use GNU toolchain.

#### **3. Rust Version Too Old**
```
error: Rust 1.92.0 or newer is required
```
**Solution:** Update Rust:
```bash
rustup update stable
```

#### **4. Cargo Build Hangs**
**Solution:** Try building with fewer threads:
```bash
cargo build --release -j 2
```

#### **5. Memory Exhausted**
**Solution:** Increase swap or build with less parallelism:
```bash
CARGO_BUILD_JOBS=2 cargo build --release
```

---

## **🧪 Testing Your Build**

### **Run Test Suite**
```bash
# Run all tests
cargo test --release

# Run specific test category
cargo test --release --test integration
cargo test --release --test unit
cargo test --release --test benchmarks
```

### **Run Benchmarks**
```bash
# Compile benchmark
./target/release/zetac compile benches/FULL_BENCHMARK.z -o benchmark

# Run benchmark
./benchmark

# Or use cargo bench
cargo bench
```

### **Verify Optimizations**
```bash
# Check if optimizations are working
./target/release/zetac compile tests/integration/test_compiler_integration.z -o test_opt
./test_opt
```

Expected output:
```
=== Zeta v0.5.0 Compiler Integration Test ===
Testing optimization systems...
1. Testing match optimization...
   Match optimization test passed
2. Testing cache-optimized HashMap...
   HashMap test passed
3. Testing memory pool system...
   Memory pool test passed
4. Testing pooled vector...
   Pooled vector test passed
5. Testing complex match optimization...
   Complex match test passed

✅ All integration tests passed!
```

---

## **📊 Build Configuration**

### **Environment Variables**
```bash
# Control parallelism
export CARGO_BUILD_JOBS=4

# Enable verbose output
export RUST_LOG=debug

# Set LLVM path
export LLVM_SYS_210_PREFIX=/usr/lib/llvm-21

# Custom target
export CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER=x86_64-linux-gnu-gcc
```

### **Cargo Configuration (~/.cargo/config.toml)**
```toml
[build]
jobs = 4  # Parallel jobs
rustflags = ["-C", "target-cpu=native"]

[target.x86_64-pc-windows-msvc]
linker = "x86_64-w64-mingw32-gcc"

[target.aarch64-apple-darwin]
linker = "aarch64-apple-darwin-clang"
```

---

## **🔗 Dependencies**

Zeta v0.5.0 depends on:
- **inkwell 0.8.0** (LLVM bindings)
- **chalk-ir 0.104.0** (trait solving)
- **rayon 1.11.0** (parallelism)
- **serde 1.0.228** (serialization)

All dependencies are automatically managed by Cargo.

---

## **🎯 Performance Tips**

### **For Faster Builds:**
1. Use `--release` flag for production builds
2. Enable sccache for compilation caching:
   ```bash
   cargo install sccache
   export RUSTC_WRAPPER=sccache
   ```
3. Use mold linker (Linux):
   ```bash
   cargo install -f cargo-binutils
   export RUSTFLAGS="-C link-arg=-fuse-ld=mold"
   ```

### **For Smaller Binaries:**
```bash
# Strip debug symbols
strip target/release/zetac

# Or build with size optimization
cargo build --release -Z build-std=std,panic_abort -Z build-std-features=panic_immediate_abort --target x86_64-unknown-linux-gnu
```

---

## **📞 Getting Help**

If you encounter build issues:

1. **Check Issues:** [GitHub Issues](https://github.com/murphsicles/zeta/issues)
2. **Community:** [Discord Server](https://discord.gg/zeta)
3. **Documentation:** [docs.zeta-lang.org](https://docs.zeta-lang.org)

**Common Solutions:**
- Update all dependencies: `cargo update`
- Clear build cache: `cargo clean`
- Check LLVM installation: `llvm-config --version`
- Verify Rust toolchain: `rustup show`

---

## **✅ Success Checklist**

After building, verify:
- [ ] `zetac --version` shows 0.5.0
- [ ] Can compile simple `.z` files
- [ ] Test suite passes
- [ ] Optimizations are enabled (check output)
- [ ] No warnings or errors in build log

---

**Happy Building!** 🏗️⚡

*Zeta v0.5.0 - The Efficiency Release*  
*Build once, optimize everywhere.*