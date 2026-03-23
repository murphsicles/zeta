# FILETREE.md - Zeta Project Structure & Navigation

## 🏗️ FIRST PRINCIPLES ORGANIZATION

**Philosophy:** Structure follows function. Every directory has a single, clear responsibility.

```
zeta/                              # ROOT: Project Dashboard
├── README.md                      # WHAT: Project overview
├── AGENTS.md                      # WHO & WHY: Project identity & philosophy
├── FILETREE.md                    # WHERE: Project structure (this file)
├── LICENSE                        # LEGAL: License terms
├── .gitignore                     # CONFIG: Git configuration
├── .github/                       # CI/CD: GitHub workflows
│   └── workflows/                 # Automated workflows
├── src/                           # SOURCE: Language implementation
│   ├── frontend/                  # Lexical analysis & parsing
│   ├── middle/                    # Type system & semantic analysis
│   ├── backend/                   # Code generation & optimization
│   └── runtime/                   # Runtime system & standard library
├── tests/                         # VERIFICATION: Testing suite
│   ├── unit/                      # Individual component tests
│   ├── integration/               # Cross-component tests
│   ├── regression/                # Regression test suite
│   └── fuzz/                      # Fuzz testing
├── benches/                       # MEASUREMENT: Performance benchmarks
│   ├── match_performance.z        # Match expression benchmarks
│   ├── hashmap_bench.z            # HashMap operation benchmarks
│   ├── string_ops.z               # String operation benchmarks
│   ├── memory_bench.z             # Memory & cache benchmarks
│   ├── FULL_BENCHMARK.z           # Comprehensive benchmark suite
│   └── OPTIMIZED_BENCHMARK.z      # Optimized implementation benchmarks
├── verify/                        # VALIDATION: Verification systems
│   ├── build/                     # Build verification
│   ├── file_state/                # File state monitoring
│   ├── recovery/                  # Recovery verification
│   └── system/                    # System verification
├── tools/                         # AUTOMATION: Development tools
│   ├── build/                     # Build automation
│   ├── ci/                        # CI/CD tools
│   ├── dev/                       # Development utilities
│   └── release/                   # Release automation
├── config/                        # SETTINGS: Configuration files
│   ├── compiler/                  # Compiler configuration
│   ├── ci/                        # CI configuration
│   ├── editor/                    # Editor configuration
│   └── lint/                      # Lint configuration
└── docs/                          # INFORMATION: Documentation
    ├── api/                       # API documentation
    ├── guides/                    # User guides & tutorials
    ├── design/                    # Design documents & decisions
    ├── examples/                  # Example code
    └── *.md                       # Top-level documentation
```

## 📁 DIRECTORY RESPONSIBILITIES

### **Root (`/`)** - Project Dashboard
- **Purpose:** High-level project information & navigation
- **Files:** Essential project metadata only
- **Rule:** No implementation, no tests, no tools

### **Source (`src/`)** - Language Implementation
- **Purpose:** Zeta language compiler & runtime
- **Subdirectories:**
  - `frontend/` - Lexer, parser, AST (syntax → structure)
  - `middle/` - Type system, semantic analysis (structure → meaning)
  - `backend/` - Code generation, optimization (meaning → machine code)
  - `runtime/` - Standard library, VM (execution environment)
- **Rule:** No tests, no benchmarks, no documentation

### **Tests (`tests/`)** - Verification Suite
- **Purpose:** Ensure correctness & prevent regressions
- **Subdirectories:**
  - `unit/` - Test individual components in isolation
  - `integration/` - Test interactions between components
  - `regression/` - Prevent reintroduction of fixed bugs
  - `fuzz/` - Find edge cases through random input
- **Rule:** No source code, no benchmarks, no tools

### **Benchmarks (`benches/`)** - Performance Measurement
- **Purpose:** Measure & optimize performance
- **Contents:** All benchmark files (language features, stdlib, memory)
- **Rule:** No tests, no source code, no verification

### **Verification (`verify/`)** - System Validation
- **Purpose:** Validate builds, monitor file state, ensure recovery
- **Subdirectories:**
  - `build/` - Verify build process & outputs
  - `file_state/` - Monitor critical files for changes
  - `recovery/` - Test recovery procedures
  - `system/` - System-level verification
- **Rule:** No tests, no benchmarks, no tools

### **Tools (`tools/`)** - Development Automation
- **Purpose:** Automate development workflows
- **Subdirectories:**
  - `build/` - Build scripts & automation
  - `ci/` - Continuous integration tools
  - `dev/` - Development utilities
  - `release/` - Release automation
- **Rule:** No source code, no tests, no configuration

### **Configuration (`config/`)** - Project Settings
- **Purpose:** Store configuration files
- **Subdirectories:**
  - `compiler/` - Compiler configuration
  - `ci/` - CI/CD configuration
  - `editor/` - Editor/IDE configuration
  - `lint/` - Linting configuration
- **Rule:** No code, no tests, no tools

### **Documentation (`docs/`)** - Project Knowledge
- **Purpose:** Document design, usage, and decisions
- **Subdirectories:**
  - `api/` - API reference documentation
  - `guides/` - Tutorials & how-to guides
  - `design/` - Design decisions & architecture
  - `examples/` - Example code & patterns
- **Rule:** No source code, no tests, no configuration

## 🔄 FILE FLOW & RELATIONSHIPS

### Development Workflow:
```
Source Code (src/) → Tests (tests/) → Benchmarks (benches/)
       ↓                    ↓                  ↓
   Compile            Verify Pass        Measure Performance
       ↓                    ↓                  ↓
   Tools (tools/) → Verification (verify/) → Documentation (docs/)
```

### Quality Assurance Chain:
1. **Compilation** (`src/` → compiler)
2. **Unit Testing** (`tests/unit/`)
3. **Integration Testing** (`tests/integration/`)
4. **Performance Validation** (`benches/`)
5. **Build Verification** (`verify/build/`)
6. **System Verification** (`verify/system/`)

## 🎯 FINDING WHAT YOU NEED

### Looking for...?
- **Language implementation?** → `src/`
- **Test a specific feature?** → `tests/unit/`
- **Test component interactions?** → `tests/integration/`
- **Measure performance?** → `benches/`
- **Build scripts?** → `tools/build/`
- **Design decisions?** → `docs/design/`
- **API reference?** → `docs/api/`
- **Example code?** → `docs/examples/`
- **Configuration?** → `config/`

### Common Paths:
- **Main test suite:** `tests/unit/test_v0.5.0_complete.z`
- **Comprehensive benchmark:** `benches/FULL_BENCHMARK.z`
- **Build script:** `tools/build/build.sh`
- **File monitoring:** `verify/file_state/file_state_monitor.ps1`
- **Recovery report:** `docs/design/RECOVERY_REPORT.md`
- **Release preparation:** `docs/design/RELEASE_PREPARATION.md`

## 🚫 WHAT GOES WHERE (Rules)

### In Root (`/`):
- ✅ Project identity & philosophy
- ✅ High-level structure & navigation
- ✅ Legal requirements
- ✅ Essential Git configuration

### NOT In Root:
- ❌ Source code implementation
- ❌ Test files
- ❌ Benchmark files
- ❌ Build scripts
- ❌ Configuration files
- ❌ Detailed documentation

## 📈 EVOLUTION & MAINTENANCE

### When Adding New Files:
1. **Identify purpose** (implementation, test, benchmark, tool, etc.)
2. **Find correct directory** based on single responsibility
3. **Follow naming conventions** within that directory
4. **Update this file** if structure changes significantly

### When Restructuring:
1. **Update this file** first (plan the structure)
2. **Move files** to new locations
3. **Update references** in source code, tests, scripts
4. **Verify nothing broke** using verification suite

### Principles for Change:
- **Clarity** > Convention
- **Function** > Form
- **Maintainability** > "The way we've always done it"
- **First Principles** > Industry standards (when they conflict)

## 🔍 QUICK NAVIGATION COMMANDS

```bash
# View this structure
cat FILETREE.md

# See actual file tree (simplified)
find . -type f -name "*.z" | head -20
find . -type f -name "*.md" | head -20

# Count files by type
find . -name "*.z" -type f | wc -l
find . -name "*.md" -type f | wc -l
find . -name "*.ps1" -type f | wc -l

# Find largest directories
du -sh */ | sort -rh
```

---

**Last Updated:** 2026-03-18  
**Version:** 1.0  
**Status:** Active Project Blueprint

*This file is the map. The code is the territory. Keep both aligned.*