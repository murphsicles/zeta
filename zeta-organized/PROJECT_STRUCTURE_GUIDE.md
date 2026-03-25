# PROJECT STRUCTURE GUIDE - AGREED 3 TIMES

## 🎯 FINAL AGREED STRUCTURE (CONFIRMED 3X)

```
zeta/
├── src/                    # Source code
│   ├── frontend/          # Lexical and syntactic analysis
│   │   ├── lexer/         # Tokenization
│   │   ├── parser/        # Parsing
│   │   ├── ast/           # Abstract syntax tree
│   │   └── diagnostics/   # Error reporting
│   │
│   ├── middle/            # Semantic analysis
│   │   ├── mir/           # Mid-level IR
│   │   ├── resolver/      # Name resolution
│   │   ├── types/         # Type checking (was typeck)
│   │   └── ownership/     # Borrow checking (was borrowck)
│   │
│   ├── backend/           # Code generation
│   │   └── codegen/       # LLVM code generation
│   │
│   ├── driver/            # Compiler driver
│   └── runtime/           # Runtime support
│
├── tests/                  # Test suite
│   ├── unit/              # Unit tests
│   ├── integration/       # Integration tests
│   └── artifacts/         # Test outputs
│
├── build/                 # Build artifacts
│   ├── executables/       # .exe files
│   ├── objects/           # .o files
│   └── intermediates/     # Other build files
│
├── docs/                  # Documentation
│   ├── planning/          # Planning documents
│   ├── architecture/      # Architecture docs
│   └── thinking/          # Thinking traces
│
├── scripts/               # Scripts
└── tools/                 # Tools and utilities
```

## 🔍 FILE CATEGORIZATION RULES

### What Goes Where:

**src/frontend/lexer/** → `*lexer*`, `*token*`, `*scan*`, `*character*`
**src/frontend/parser/** → `*parser*`, `*expr*`, `*stmt*`, `top_level*`
**src/frontend/ast/** → `*ast*`, `*node*`, type definitions
**src/middle/types/** → `*type*`, `*check*`, type system
**src/middle/ownership/** → `*borrow*`, `*lifetime*`, ownership system
**src/middle/mir/** → `*mir*`, `*lower*`, MIR
**src/backend/codegen/** → `*codegen*`, `*emit*`, `*generate*`
**src/driver/** → `*main*`, `*driver*`, `*cli*`
**src/runtime/** → `*runtime*`, `*memory*`, `*actor*`

## 🚨 CURRENT REALITY CHECK

### Current `zeta\` directory has:
- **100+ files in root** - Mixed executables, sources, docs, tests
- **Build artifacts everywhere** - .exe, .o files scattered
- **Documentation mixed** - Planning docs with source files
- **Test files scattered** - No test directory structure
- **Thinking traces** - Should be in docs/thinking/

### What Needs to Happen:
1. **YOU move files** to organized structure (I can't with current permissions)
2. **I provide mapping** of where each file should go
3. **We verify** structure matches agreed plan

## 🔧 IMMEDIATE ACTION

### For You to Run (One Time):
```powershell
# 1. Create directory structure
cd C:\Users\mummy\Documents\DarkFactory\zeta
mkdir src, tests, build, docs, scripts -Force
mkdir src\frontend, src\middle, src\backend, src\driver, src\runtime -Force
mkdir src\frontend\lexer, src\frontend\parser, src\frontend\ast, src\frontend\diagnostics -Force
mkdir src\middle\mir, src\middle\resolver, src\middle\types, src\middle\ownership -Force
mkdir src\backend\codegen -Force
mkdir tests\unit, tests\integration, tests\artifacts -Force
mkdir build\executables, build\objects, build\intermediates -Force
mkdir docs\planning, docs\architecture, docs\thinking -Force

# 2. Move build artifacts
Move-Item *.exe build\executables\ -ErrorAction SilentlyContinue
Move-Item *.exe.o build\objects\ -ErrorAction SilentlyContinue
Move-Item *.o build\objects\ -ErrorAction SilentlyContinue

# 3. Move documentation
Move-Item *.md docs\ -ErrorAction SilentlyContinue
Move-Item *SUMMARY* docs\planning\ -ErrorAction SilentlyContinue
Move-Item *PLAN* docs\planning\ -ErrorAction SilentlyContinue
Move-Item *LOG* docs\planning\ -ErrorAction SilentlyContinue

# 4. Move scripts
Move-Item *.ps1 scripts\ -ErrorAction SilentlyContinue
Move-Item *.bat scripts\ -ErrorAction SilentlyContinue
Move-Item *.sh scripts\ -ErrorAction SilentlyContinue

# 5. Delete temporary files
Remove-Item *.bak -ErrorAction SilentlyContinue
```

### Then I Can:
1. **Organize source files** within the structure
2. **Create missing files** in correct locations
3. **Verify compilation** works with new structure
4. **Continue development** properly

## 🏭 THE FIX

**You do the file operations (move/delete) once.**
**I do everything else (organization, development).**

**No more scripts. No more babysitting.**
**One-time manual fix, then I work properly.** 🏭🔧🚀