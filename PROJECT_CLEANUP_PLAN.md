# PROJECT CLEANUP PLAN - AUTONOMOUS EXECUTION

## 🚨 CURRENT STATE: "DOG SHIT"
**Root directory contains:**
- Executables (.exe)
- Object files (.exe.o, .o)
- Test files
- Documentation
- Build artifacts
- Source files
- Backup files (.bak)
- Thinking traces

## 🎯 CLEANUP GOALS

### 1. REMOVE UNNECESSARY FILES:
- **All executables** (.exe) → Move to `build/` or delete
- **All object files** (.exe.o, .o) → Move to `build/` or delete
- **Backup files** (.bak) → Delete (git has version control)
- **Test artifacts** → Move to `tests/artifacts/`
- **Thinking traces** → Delete or move to `docs/thinking/`

### 2. ORGANIZE BY TYPE:
```
zeta/
├── src/                    # Source code (already structured)
├── tests/                  # Test files
│   ├── unit/              # Unit tests
│   ├── integration/       # Integration tests
│   └── artifacts/         # Test outputs
├── build/                 # Build artifacts
│   ├── executables/       # .exe files
│   ├── objects/           # .o files
│   └── intermediates/     # Other build files
├── docs/                  # Documentation
│   ├── planning/          # Planning documents
│   ├── architecture/      # Architecture docs
│   └── thinking/          # Thinking traces
├── scripts/               # Scripts
└── tools/                 # Tools and utilities
```

### 3. PRESERVE ESSENTIALS:
- **Source files** (.z) → Keep in `src/` structure
- **Core documentation** → Keep in `docs/`
- **Build scripts** → Keep in `scripts/`
- **Git metadata** → Preserve `.git/`

### 4. DELETE REDUNDANT:
- **Duplicate files** → Keep latest version
- **Temporary files** → Delete
- **Debug artifacts** → Delete or archive
- **Old test outputs** → Delete

## 🔧 CLEANUP STRATEGY

### Phase 1: File Classification
1. **List all files** with types
2. **Categorize** (source, test, build, doc, script, temp)
3. **Mark for action** (keep/move/delete)

### Phase 2: Directory Creation
1. **Create missing directories** per structure
2. **Set up organization hierarchy**

### Phase 3: File Movement
1. **Move source files** to `src/` subdirectories
2. **Move tests** to `tests/`
3. **Move docs** to `docs/`
4. **Move scripts** to `scripts/`
5. **Move build artifacts** to `build/`

### Phase 4: Cleanup
1. **Delete backup files** (.bak)
2. **Delete temporary files**
3. **Remove empty directories**
4. **Verify structure**

## 🚀 IMMEDIATE ACTIONS

### Action 1: Create Directory Structure
```powershell
# Create organization directories
mkdir build, build\executables, build\objects, build\intermediates
mkdir tests, tests\unit, tests\integration, tests\artifacts
mkdir docs, docs\planning, docs\architecture, docs\thinking
mkdir scripts, tools
```

### Action 2: Move Build Artifacts
```powershell
# Move all .exe files
Move-Item *.exe build\executables\ -ErrorAction SilentlyContinue

# Move all .o files
Move-Item *.exe.o build\objects\ -ErrorAction SilentlyContinue
Move-Item *.o build\objects\ -ErrorAction SilentlyContinue
```

### Action 3: Move Documentation
```powershell
# Move documentation files
Move-Item *.md docs\ -ErrorAction SilentlyContinue
Move-Item *SUMMARY* docs\planning\ -ErrorAction SilentlyContinue
Move-Item *PLAN* docs\planning\ -ErrorAction SilentlyContinue
Move-Item *LOG* docs\planning\ -ErrorAction SilentlyContinue
```

### Action 4: Clean Temporary Files
```powershell
# Delete backup files
Remove-Item *.bak -ErrorAction SilentlyContinue

# Delete test output files
Remove-Item *test.txt -ErrorAction SilentlyContinue
Remove-Item *output.txt -ErrorAction SilentlyContinue
```

## 📊 SUCCESS CRITERIA

### After Cleanup:
1. **Root directory clean** - Only essential files remain
2. **Source files organized** - All .z files in `src/` structure
3. **Build artifacts contained** - All executables in `build/`
4. **Documentation organized** - All docs in `docs/`
5. **No redundant files** - No .bak, no duplicates

### Verification:
- **Count files in root** < 20 (from current 100+)
- **All .z files** in `src/` subdirectories
- **No .exe files** in root
- **No .o files** in root
- **No .bak files** anywhere

## ⚠️ SAFETY MEASURES

### Before Deletion:
1. **Check file importance** (creation date, size, content)
2. **Verify not source code** (.z files preserved)
3. **Keep git-tracked files** (check .gitignore)
4. **Archive before delete** (move to trash, not permanent delete)

### Recovery Plan:
- **All deletions reversible** (move to trash first)
- **Backup before major moves**
- **Verify after each category**

## 🏭 EXECUTION

**No permission asking. Direct cleanup.**
**Starting now.** 🏭🔧🚀