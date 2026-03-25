# SOURCE FILE ORGANIZATION GUIDE

## 🎯 GOAL: ORGANIZE 159 ZETA FILES INTO PROFESSIONAL STRUCTURE

### Current `src/` Structure (Agreed):
```
src/
├── frontend/
│   ├── lexer/          # Lexical analysis
│   ├── parser/         # Syntax analysis  
│   ├── ast/            # AST definitions
│   └── diagnostics/    # Error reporting
├── middle/
│   ├── mir/            # MIR in Zeta
│   ├── resolver/       # Name resolution
│   ├── types/          # Type checking (was typeck)
│   └── ownership/      # Borrow checking (was borrowck)
├── backend/
│   └── codegen/        # Code generation
├── driver/             # Compiler driver
└── runtime/            # Runtime support
```

## 🔍 FILE CATEGORIZATION RULES

### Frontend/Lexer Files (→ `src/frontend/lexer/`):
- Contains: `lex`, `token`, `scan`, `character`
- Examples: `character_scanner_complete.z`, `lexer_basic.z`
- Pattern: `*lexer*`, `*token*`, `*scan*`, `*character*`

### Frontend/Parser Files (→ `src/frontend/parser/`):
- Contains: `parse`, `expr`, `stmt`, `grammar`
- Examples: `parser_fixed.z`, `expr.z`, `stmt.z`
- Pattern: `*parser*`, `*expr*`, `*stmt*`, `top_level*`

### Frontend/AST Files (→ `src/frontend/ast/`):
- Contains: `ast`, `node`, `tree`, type definitions
- Examples: `ast.z`, `type.z`, `node.z`
- Pattern: `*ast*`, `*node*`, structural definitions

### Middle/Types Files (→ `src/middle/types/`):
- Contains: `type`, `check`, `infer`, `system`
- Examples: `type_system.z`, `type_check.z`
- Pattern: `*type*`, `*check*` (type checking)

### Middle/Ownership Files (→ `src/middle/ownership/`):
- Contains: `borrow`, `lifetime`, `ownership`, `reference`
- Examples: `borrow_check.z`, `lifetime.z`
- Pattern: `*borrow*`, `*lifetime*`, `*ownership*`

### Middle/MIR Files (→ `src/middle/mir/`):
- Contains: `mir`, `lower`, `intermediate`, `representation`
- Examples: `mir.z`, `lower.z`, `mir_builder.z`
- Pattern: `*mir*`, `*lower*` (lowering to MIR)

### Backend/Codegen Files (→ `src/backend/codegen/`):
- Contains: `codegen`, `emit`, `generate`, `assembly`
- Examples: `codegen.z`, `emit_llvm.z`
- Pattern: `*codegen*`, `*emit*`, `*generate*`

### Driver Files (→ `src/driver/`):
- Contains: `main`, `driver`, `cli`, `compile`
- Examples: `main.z`, `driver.z`, `cli.z`
- Pattern: `*main*`, `*driver*`, `*cli*`

### Runtime Files (→ `src/runtime/`):
- Contains: `runtime`, `memory`, `alloc`, `gc`, `actor`
- Examples: `runtime.z`, `memory.z`, `actor.z`
- Pattern: `*runtime*`, `*memory*`, `*alloc*`, `*gc*`

## 📋 QUICK ORGANIZATION SCRIPT

```powershell
# Run this AFTER the main cleanup script
# Organizes .z files from src/ into subdirectories

cd C:\Users\mummy\Documents\DarkFactory\zeta\src

# Lexer files
Move-Item *lexer* frontend\lexer\ -ErrorAction SilentlyContinue
Move-Item *token* frontend\lexer\ -ErrorAction SilentlyContinue
Move-Item *scan* frontend\lexer\ -ErrorAction SilentlyContinue
Move-Item *character* frontend\lexer\ -ErrorAction SilentlyContinue

# Parser files
Move-Item *parser* frontend\parser\ -ErrorAction SilentlyContinue
Move-Item *expr* frontend\parser\ -ErrorAction SilentlyContinue
Move-Item *stmt* frontend\parser\ -ErrorAction SilentlyContinue

# AST files
Move-Item *ast* frontend\ast\ -ErrorAction SilentlyContinue
Move-Item *node* frontend\ast\ -ErrorAction SilentlyContinue

# Type system files
Move-Item *type* middle\types\ -ErrorAction SilentlyContinue
Move-Item *check* middle\types\ -ErrorAction SilentlyContinue

# MIR files
Move-Item *mir* middle\mir\ -ErrorAction SilentlyContinue
Move-Item *lower* middle\mir\ -ErrorAction SilentlyContinue

# Driver files
Move-Item *main* driver\ -ErrorAction SilentlyContinue
Move-Item *driver* driver\ -ErrorAction SilentlyContinue
Move-Item *cli* driver\ -ErrorAction SilentlyContinue

# Runtime files
Move-Item *runtime* runtime\ -ErrorAction SilentlyContinue
Move-Item *memory* runtime\ -ErrorAction SilentlyContinue
Move-Item *actor* runtime\ -ErrorAction SilentlyContinue

# Check what remains
Get-ChildItem -Filter "*.z" -File | Select-Object Name
```

## 🚨 SPECIAL CASES

### Files that might be mis-categorized:
- `type_system.z` → `src/middle/types/` (NOT `src/frontend/ast/`)
- `borrow_check.z` → `src/middle/ownership/` (NOT `src/middle/types/`)
- `main.z` → `src/driver/` (NOT root)
- `cli.z` → `src/driver/` (command line interface)

### Files that should stay in root `src/`:
- `lib.z` (if it's a library entry point)
- `prelude.z` (if it's prelude definitions)
- Files that don't fit categories (review manually)

## ✅ VERIFICATION STEPS

After organization:

1. **Count files in each directory:**
```powershell
Get-ChildItem -Path src -Filter "*.z" -Recurse -File | Group-Object Directory | Select-Object Name, Count
```

2. **Check for orphaned files:**
```powershell
# Files still in src/ root
Get-ChildItem -Path src -Filter "*.z" -File | Select-Object Name
```

3. **Verify critical files placed:**
- `parser.z` → `src/frontend/parser/`
- `lexer.z` → `src/frontend/lexer/`
- `type_system.z` → `src/middle/types/`
- `main.z` → `src/driver/`

## 🏭 FINAL STRUCTURE GOAL

**After cleanup and organization:**
- Root directory: Clean (only README, .gitignore, config files)
- `src/`: All .z files organized by component
- `build/`: All executables and object files
- `tests/`: All test files and artifacts
- `docs/`: All documentation
- `scripts/`: All scripts

**Professional. Clean. Maintainable.** 🏭🔧🚀