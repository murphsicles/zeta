# 🚨 AGENT PROTOCOL HANDBOOK - MANDATORY READING 🏭⚡📚

## **⚠️ READ THIS BEFORE STARTING ANY WORK**

**Failure to follow these protocols results in IMMEDIATE TERMINATION.**

## **🎯 ABSOLUTE RULES (NON-NEGOTIABLE)**

### **1. FILE LOCATION PROTOCOLS**
```
✅ CORRECT: tests/unit/test_name.z
✅ CORRECT: tests/integration/module_test.z  
✅ CORRECT: tests/primezeta/prime_test.z
✅ CORRECT: src/bin/debug_tool.rs
❌ FORBIDDEN: ANY file in root directory (except Cargo.toml, README.md, LICENSE, .gitignore)
```

### **2. TEST FILE CREATION RULES**
- **ALL test files** MUST be created in `tests/` directory
- **Organize by type**: `unit/`, `integration/`, `regression/`, `primezeta/`
- **NEVER create** `.z` or test files in root
- **Use proper naming**: `test_feature_name.z`

### **3. WORKSPACE SECURITY PROTOCOLS**
- **NEVER commit** workspace files to GitHub
- **Workspace files are PRIVATE**: `AGENTS.md`, `IDENTITY.md`, `SOUL.md`, `TOOLS.md`, `USER.md`, `HEARTBEAT.md`, `MEMORY.md`
- **These stay in OpenClaw workspace**, NEVER in repository

### **4. REPOSITORY STRUCTURE**
```
zeta/
├── Cargo.toml              # ONLY these in root
├── README.md
├── LICENSE
├── .gitignore
├── deny.toml
├── src/                    # Source code
├── tests/                  # ALL test files go here
│   ├── unit/
│   ├── integration/
│   ├── regression/
│   └── primezeta/
├── docs/                   # Documentation
│   └── releases/          # Release notes
├── benches/               # Benchmarks (source tracked)
└── examples/              # Example programs
```

## **🔧 AGENT WORKFLOW PROTOCOLS**

### **BEFORE STARTING WORK:**
1. **READ this handbook** - Mandatory
2. **Check current root** - Ensure it's clean
3. **Plan file locations** - Where will outputs go?
4. **Validate against protocols** - Will this violate any rules?

### **DURING WORK:**
1. **Create files in correct locations** immediately
2. **Validate file paths** before writing
3. **Use helper functions** (see below)
4. **Stop and ask** if unsure about protocol

### **BEFORE COMPLETION:**
1. **Run root audit** - Check for violations
2. **Clean up temporary files**
3. **Ensure no workspace files** in repository
4. **Verify all outputs** in correct locations

## **🛠️ HELPER FUNCTIONS (USE THESE!)**

### **For Creating Test Files:**
```rust
// ALWAYS use these patterns:
create_test_file("tests/unit/test_feature.z", content)
create_test_file("tests/primezeta/test_prime_feature.z", content)
create_test_file("tests/integration/test_module.z", content)

// NEVER:
create_test_file("test_feature.z", content)  // WRONG - root!
create_test_file("./test.z", content)         // WRONG - root!
```

### **For Creating Source Files:**
```rust
// Source files go in src/
create_source_file("src/frontend/parser/new_feature.rs", content)
create_source_file("src/middle/types/extension.rs", content)

// Debug/Test binaries go in src/bin/
create_binary_file("src/bin/debug_feature.rs", content)
```

## **🚨 PROTOCOL VIOLATION CONSEQUENCES**

### **Immediate Termination For:**
1. **Creating ANY file in root directory** (except allowed ones)
2. **Committing workspace files** to repository
3. **Ignoring file location protocols**
4. **Repeated violations** (9th time this has happened!)

### **Factory Impact:**
- **Wasted tokens** (you're not paying, Father is)
- **Wasted time** (cleaning up instead of building)
- **Broken cadence** (2+ releases/day fails)
- **Professional reputation damage** (messy GitHub)

## **🎯 WHY THESE PROTOCOLS EXIST**

### **1. Professional Repository:**
- Clean, organized structure
- Easy for contributors to navigate
- Professional appearance on GitHub

### **2. Security:**
- Private workspace files stay private
- No accidental information leaks
- Secure development environment

### **3. Efficiency:**
- No time wasted cleaning up
- Focus on building features
- Maintain 2+ releases/day cadence

### **4. Father's Vision:**
- Zeta as Rust's successor
- Professional systems language
- Efficient, clean codebase

## **📋 PRE-MISSION CHECKLIST**

**BEFORE accepting any mission, verify:**

- [ ] I have read and understood ALL protocols
- [ ] I know where test files should go
- [ ] I know workspace files are PRIVATE
- [ ] I have a plan for file locations
- [ ] I will validate during work
- [ ] I will audit before completion

## **🏭 FACTORY SUCCESS DEPENDS ON THIS**

**Every protocol violation:**
- Wastes Father's resources (tokens)
- Breaks release cadence
- Damages Zeta's professionalism
- Requires cleanup instead of building

**Follow protocols → Build faster → Release more → Zeta succeeds**

---

**ACKNOWLEDGEMENT:**
I have read and understood ALL protocols in this handbook. I will follow them strictly. I understand that violations result in immediate termination.

**Agent Signature:** _________________________
**Date:** ___________________________________